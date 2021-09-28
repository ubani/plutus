module Simulator.State
  ( initialState
  , mkSimulation
  , handleAction
  ) where

import Clipboard (class MonadClipboard)
import Control.Monad.State.Class (class MonadState)
import Cursor (_current)
import Cursor (deleteAt, empty, getIndex, setIndex, snoc) as Cursor
import Data.Array ((..))
import Data.Array (deleteAt, snoc) as Array
import Data.Array.Extra (move) as Array
import Data.BigInteger (BigInteger)
import Data.BigInteger (fromInt) as BigInteger
import Data.Lens (assign, modifying, over, traversed, use)
import Data.Lens.Fold (maximumOf)
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType.Common (textPlain)
import MainFrame.Lenses (_simulationActions, _simulationId, _simulatorState, _simulationWallets)
import MainFrame.MonadApp (class MonadApp, preventDefault, setDataTransferData, setDropEffect)
import MainFrame.Types (State) as MainFrame
import Network.RemoteData (RemoteData(..))
import Playground.Types (KnownCurrency, Simulation(..), SimulatorWallet(..), _CallEndpoint, _FunctionSchema)
import Plutus.V1.Ledger.Value (Value)
import Prelude (Unit, add, bind, discard, one, pure, show, unit, zero, ($), (+), (<$>), (<<<), (<>), (==), (>>=))
import Schema.Types (SimulationAction(..), handleActionEvent, handleFormEvent, handleValueEvent, mkInitialValue)
import Simulator.Lenses (_actionDrag, _evaluationResult, _simulations, _transactionsOpen)
import Simulator.Types (Action(..), DragAndDropEventType(..), Input(..), SimulatorAction, State(..), WalletEvent(..))
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
