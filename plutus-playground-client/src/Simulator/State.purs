module Simulator.State
  ( initialState
  , mkSimulation
  , handleAction
  ) where

import AjaxUtils (ajaxErrorRefLabel)
import Animation (class MonadAnimate, animate)
import Chain.State (handleAction) as Chain
import Chain.Types (Action(..), initialState) as Chain
import Chain.Types (AnnotatedBlockchain(..), _chainFocusAppearing, _txIdOf)
import Clipboard (class MonadClipboard)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.State.Class (class MonadState, gets)
import Control.Monad.State.Extra (zoomStateT)
import Control.Monad.Trans.Class (lift)
import Cursor (_current)
import Cursor (deleteAt, empty, getIndex, setIndex, snoc) as Cursor
import Data.Array ((..))
import Data.Array (deleteAt, snoc) as Array
import Data.Array.Extra (move) as Array
import Data.BigInteger (BigInteger)
import Data.BigInteger (fromInt) as BigInteger
import Data.Either (Either(..))
import Data.Lens (assign, modifying, over, to, traversed, use)
import Data.Lens.Extra (peruse)
import Data.Lens.Fold (lastOf, maximumOf)
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType.Common (textPlain)
import Data.RawJson (RawJson(..))
import Data.Traversable (traverse)
import Foreign.Generic (encodeJSON)
import Language.Haskell.Interpreter (SourceCode)
import MainFrame.MonadApp (class MonadApp, editorGetContents, postEvaluation, preventDefault, scrollIntoView, setDataTransferData, setDropEffect)
import MainFrame.Lenses (_simulatorState)
import MainFrame.Types (State) as MainFrame
import Network.RemoteData (RemoteData(..), isSuccess)
import Playground.Types (ContractCall(..), Evaluation(..), KnownCurrency, Simulation(..), SimulatorWallet(..), _CallEndpoint, _FunctionSchema)
import Plutus.V1.Ledger.Value (Value)
import Prelude (class Applicative, Unit, add, bind, discard, identity, join, one, pure, show, unit, void, when, zero, ($), (+), (<$>), (<<<), (<>), (<*>), (==), (>>=))
import Schema.Types (Expression, FormArgument, SimulationAction(..), formArgumentToJson, handleActionEvent, handleFormEvent, handleValueEvent, mkInitialValue, traverseFunctionSchema)
import Simulator.Lenses (_actionDrag, _blockchainVisualisationState, _evaluationResult, _lastEvaluatedSimulation, _resultRollup, _simulationActions, _simulations, _simulationId, _simulationWallets, _successfulEvaluationResult, _transactionsOpen)
import Simulator.Types (Action(..), DragAndDropEventType(..), Input(..), SimulatorAction, State(..), WalletEvent(..))
import Simulator.View (simulatorTitleRefLabel, simulationsErrorRefLabel)
import Validation (_argumentValues, _argument)
import Wallet.Emulator.Wallet (WalletNumber(..))
import Wallet.Lenses (_simulatorWalletBalance, _simulatorWalletWallet, _walletId)
import Web.HTML.Event.DataTransfer as DataTransfer

initialState :: State
initialState =
  State
    { simulations: Cursor.empty
    , actionDrag: Nothing
    , evaluationResult: NotAsked
    , lastEvaluatedSimulation: Nothing
    , transactionsOpen: false
    , blockchainVisualisationState: Chain.initialState
    }

mkSimulation :: Array KnownCurrency -> Int -> Simulation
mkSimulation simulationCurrencies simulationId =
  Simulation
    { simulationName: "Simulation " <> show simulationId
    , simulationId
    , simulationActions: []
    , simulationWallets: mkSimulatorWallet simulationCurrencies <<< BigInteger.fromInt <$> 1 .. 2
    }

mkSimulatorWallet :: Array KnownCurrency -> BigInteger -> SimulatorWallet
mkSimulatorWallet currencies walletId =
  SimulatorWallet
    { simulatorWalletWallet: WalletNumber { getWallet: walletId }
    , simulatorWalletBalance: mkInitialValue currencies (BigInteger.fromInt 100_000_000)
    }

handleAction ::
  forall m.
  MonadState MainFrame.State m =>
  MonadClipboard m =>
  MonadAnimate m MainFrame.State =>
  MonadApp m =>
  Input -> Action -> m Unit
-- Note: the following three cases involve some temporary fudges that should become
-- unnecessary when we remodel and have one evaluationResult per simulation. In
-- particular: we prevent simulation changes while the evaluationResult is Loading,
-- and switch to the simulations view (from transactions) following any change
handleAction input@(Input { knownCurrencies }) AddSimulationSlot = do
  evaluationResult <- use (_simulatorState <<< _evaluationResult)
  case evaluationResult of
    Loading -> pure unit
    _ -> do
      modifying (_simulatorState <<< _simulations)
        ( \simulations ->
            let
              maxsimulationId = fromMaybe 0 $ maximumOf (traversed <<< _simulationId) simulations

              simulationId = maxsimulationId + 1
            in
              Cursor.snoc simulations $ mkSimulation knownCurrencies simulationId
        )
      assign (_simulatorState <<< _transactionsOpen) false

handleAction _ (SetSimulationSlot index) = do
  evaluationResult <- use (_simulatorState <<< _evaluationResult)
  case evaluationResult of
    Loading -> pure unit
    _ -> do
      modifying (_simulatorState <<< _simulations) $ Cursor.setIndex index
      assign (_simulatorState <<< _transactionsOpen) false

handleAction _ (RemoveSimulationSlot index) = do
  evaluationResult <- use (_simulatorState <<< _evaluationResult)
  case evaluationResult of
    Loading -> pure unit
    _ -> do
      simulations <- use (_simulatorState <<< _simulations)
      if (Cursor.getIndex simulations) == index then
        assign (_simulatorState <<< _transactionsOpen) false
      else
        pure unit
      modifying (_simulatorState <<< _simulations) $ Cursor.deleteAt index

handleAction _ (ActionDragAndDrop index DragStart event) = do
  setDataTransferData event textPlain (show index)
  assign (_simulatorState <<< _actionDrag) $ Just index

handleAction _ (ActionDragAndDrop _ DragEnd event) = assign (_simulatorState <<< _actionDrag) Nothing

handleAction _ (ActionDragAndDrop _ DragEnter event) = do
  preventDefault event
  setDropEffect DataTransfer.Move event

handleAction _ (ActionDragAndDrop _ DragOver event) = do
  preventDefault event
  setDropEffect DataTransfer.Move event

handleAction _ (ActionDragAndDrop _ DragLeave event) = pure unit

handleAction _ (ActionDragAndDrop destination Drop event) = do
  use (_simulatorState <<< _actionDrag)
    >>= case _ of
        Just source -> modifying (_simulatorState <<< _simulations <<< _current <<< _simulationActions) (Array.move source destination)
        _ -> pure unit
  preventDefault event
  assign (_simulatorState <<< _actionDrag) Nothing

handleAction input@(Input { knownCurrencies }) (ModifyWallets action) = modifying (_simulatorState <<< _simulations <<< _current <<< _simulationWallets) (handleActionWalletEvent (mkSimulatorWallet knownCurrencies) action)

handleAction input@(Input { knownCurrencies }) (ChangeSimulation subaction) =
  let
    initialValue = mkInitialValue knownCurrencies zero
  in
    modifying (_simulatorState <<< _simulations <<< _current <<< _simulationActions) (handleSimulationAction initialValue subaction)

handleAction _ (SetTransactionsOpen transactionsOpen) = assign (_simulatorState <<< _transactionsOpen) transactionsOpen

handleAction _ EvaluateActions =
  void
    $ runMaybeT
    $ do
        simulation <- peruse (_simulatorState <<< _simulations <<< _current)
        evaluation <-
          MaybeT do
            contents <- editorGetContents
            pure $ join $ toEvaluation <$> contents <*> simulation
        assign (_simulatorState <<< _evaluationResult) Loading
        result <- lift $ postEvaluation evaluation
        assign (_simulatorState <<< _evaluationResult) result
        case result of
          Success (Right _) -> do
            -- on successful evaluation, update last evaluated simulation, and reset and show transactions
            when (isSuccess result) do
              assign (_simulatorState <<< _lastEvaluatedSimulation) simulation
              assign (_simulatorState <<< _blockchainVisualisationState) Chain.initialState
              -- preselect the first transaction (if any)
              mAnnotatedBlockchain <- peruse (_simulatorState <<< _successfulEvaluationResult <<< _resultRollup <<< to AnnotatedBlockchain)
              txId <- (gets <<< lastOf) (_simulatorState <<< _successfulEvaluationResult <<< _resultRollup <<< traversed <<< traversed <<< _txIdOf)
              lift $ zoomStateT (_simulatorState <<< _blockchainVisualisationState) $ Chain.handleAction (Chain.FocusTx txId) mAnnotatedBlockchain
            when (isSuccess result) (assign (_simulatorState <<< _transactionsOpen) true)
            lift $ scrollIntoView simulatorTitleRefLabel
          Success (Left _) -> do
            -- on failed evaluation, scroll the error pane into view
            lift $ scrollIntoView simulationsErrorRefLabel
          Failure _ -> do
            -- on failed response, scroll the ajax error pane into view
            lift $ scrollIntoView ajaxErrorRefLabel
          _ -> pure unit
        pure unit

handleAction _ (ChainAction chainAction) = do
  mAnnotatedBlockchain <-
    peruse (_simulatorState <<< _successfulEvaluationResult <<< _resultRollup <<< to AnnotatedBlockchain)
  let
    wrapper = case chainAction of
      (Chain.FocusTx _) -> animate (_simulatorState <<< _blockchainVisualisationState <<< _chainFocusAppearing)
      _ -> identity
  wrapper
    $ zoomStateT (_simulatorState <<< _blockchainVisualisationState)
    $ Chain.handleAction chainAction mAnnotatedBlockchain

------------------------------------------------------------
handleActionWalletEvent :: (BigInteger -> SimulatorWallet) -> WalletEvent -> Array SimulatorWallet -> Array SimulatorWallet
handleActionWalletEvent mkWallet AddWallet wallets =
  let
    maxWalletId = fromMaybe zero $ maximumOf (traversed <<< _simulatorWalletWallet <<< _walletId) wallets

    newWallet = mkWallet (add one maxWalletId)
  in
    Array.snoc wallets newWallet

handleActionWalletEvent _ (RemoveWallet index) wallets = fromMaybe wallets $ Array.deleteAt index wallets

handleActionWalletEvent _ (ModifyBalance walletIndex action) wallets =
  over
    (ix walletIndex <<< _simulatorWalletBalance)
    (handleValueEvent action)
    wallets

handleSimulationAction :: Value -> SimulationAction -> Array SimulatorAction -> Array SimulatorAction
handleSimulationAction _ (ModifyActions actionEvent) = handleActionEvent actionEvent

handleSimulationAction initialValue (PopulateAction n event) = do
  over
    ( ix n
        <<< _CallEndpoint
        <<< _argumentValues
        <<< _FunctionSchema
        <<< _argument
    )
    $ handleFormEvent initialValue event

------------------------------------------------------------
toEvaluation :: SourceCode -> Simulation -> Maybe Evaluation
toEvaluation sourceCode (Simulation { simulationActions, simulationWallets }) = do
  program <- RawJson <<< encodeJSON <$> traverse toExpression simulationActions
  pure
    $ Evaluation
        { wallets: simulationWallets
        , program
        , sourceCode
        }

toExpression :: ContractCall FormArgument -> Maybe Expression
toExpression = traverseContractCall encodeForm
  where
  encodeForm :: FormArgument -> Maybe RawJson
  encodeForm argument = (RawJson <<< encodeJSON) <$> formArgumentToJson argument

traverseContractCall ::
  forall m b a.
  Applicative m =>
  (a -> m b) ->
  ContractCall a -> m (ContractCall b)
traverseContractCall _ (AddBlocks addBlocks) = pure $ AddBlocks addBlocks

traverseContractCall _ (AddBlocksUntil addBlocksUntil) = pure $ AddBlocksUntil addBlocksUntil

traverseContractCall _ (PayToWallet payToWallet) = pure $ PayToWallet payToWallet

traverseContractCall f (CallEndpoint { caller, argumentValues: oldArgumentValues }) = rewrap <$> traverseFunctionSchema f oldArgumentValues
  where
  rewrap newArgumentValues = CallEndpoint { caller, argumentValues: newArgumentValues }
