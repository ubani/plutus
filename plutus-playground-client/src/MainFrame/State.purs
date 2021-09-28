module MainFrame.State
  ( mkMainFrame
  , handleAction
  , mkInitialState
  ) where

import AjaxUtils (AjaxErrorPaneAction(..), ajaxErrorRefLabel, renderForeignErrors)
import Analytics (analyticsTracking)
import Animation (class MonadAnimate, animate)
import Chain.State (handleAction) as Chain
import Chain.Types (Action(..), AnnotatedBlockchain(..), _chainFocusAppearing, _txIdOf)
import Chain.Types (initialState) as Chain
import Clipboard (class MonadClipboard)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Error.Extra (mapError)
import Control.Monad.Except.Extra (noteT)
import Control.Monad.Except.Trans (ExceptT(..), except, mapExceptT, withExceptT, runExceptT)
import Control.Monad.Maybe.Extra (hoistMaybe)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Reader (class MonadAsk, runReaderT)
import Control.Monad.State.Class (class MonadState, gets)
import Control.Monad.State.Extra (zoomStateT)
import Control.Monad.Trans.Class (lift)
import Cursor (_current)
import Cursor as Cursor
import Data.Array (catMaybes)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Lens (Traversal', _Right, assign, modifying, to, traversed, use, view)
import Data.Lens.Extra (peruse)
import Data.Lens.Fold (lastOf, preview)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.RawJson (RawJson(..))
import Data.String as String
import Data.Traversable (traverse)
import Editor.Lenses (_currentCodeIsCompiled, _feedbackPaneMinimised, _lastCompiledCode)
import Editor.State (initialState) as Editor
import Editor.Types (Action(..), State) as Editor
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, error)
import Foreign.Generic (decodeJSON, encodeJSON)
import Gist (_GistId, gistId)
import Gists.Types (GistAction(..))
import Gists.Types as Gists
import Halogen (Component, hoist)
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.Query (HalogenM)
import Language.Haskell.Interpreter (CompilationError(..), InterpreterError(..), InterpreterResult, SourceCode(..), _InterpreterResult)
import MainFrame.Lenses (_authStatus, _blockchainVisualisationState, _compilationResult, _contractDemos, _createGistResult, _currentDemoName, _currentView, _demoFilesMenuVisible, _editorState, _functionSchema, _gistErrorPaneVisible, _gistUrl, _knownCurrencies, _result, _resultRollup, _simulatorState, getKnownCurrencies)
import MainFrame.MonadApp (class MonadApp, editorGetContents, editorHandleAction, editorSetAnnotations, editorSetContents, getGistByGistId, getOauthStatus, postContract, postEvaluation, postGist, postGistByGistId, resizeBalancesChart, resizeEditor, runHalogenApp, saveBuffer, scrollIntoView)
import MainFrame.Types (ChildSlots, HAction(..), Query, State(..), View(..))
import MainFrame.View (render)
import Monaco (IMarkerData, markerSeverity)
import Network.RemoteData (RemoteData(..), _Success, isSuccess)
import Playground.Gists (mkNewGist, playgroundGistFile, simulationGistFile)
import Playground.Server (SPParams_(..))
import Playground.Types (ContractCall(..), ContractDemo(..), Evaluation(..), Simulation(..))
import Prelude (class Applicative, Unit, Void, bind, const, discard, flip, identity, join, mempty, not, pure, show, unit, unless, void, when, ($), (&&), (<$>), (<*>), (<<<), (<>), (==))
import Schema.Types (Expression, FormArgument, formArgumentToJson, traverseFunctionSchema)
import Servant.PureScript.Ajax (errorToString)
import Servant.PureScript.Settings (SPSettings_, defaultSettings)
import Simulator.Lenses (_evaluationResult, _lastEvaluatedSimulation, _simulations, _successfulEvaluationResult)
import Simulator.State (handleAction, initialState, mkSimulation) as Simulator
import Simulator.Types (Input(..)) as Simulator
import Simulator.View (simulatorTitleRefLabel, simulationsErrorRefLabel)
import StaticData (mkContractDemos, lookupContractDemo)
import Types (WebData)

mkInitialState :: forall m. MonadThrow Error m => Editor.State -> m State
mkInitialState editorState = do
  contractDemos <- mapError (\e -> error $ "Could not load demo scripts. Parsing errors: " <> show e) mkContractDemos
  pure
    $ State
        { demoFilesMenuVisible: false
        , gistErrorPaneVisible: true
        , currentView: Editor
        , editorState
        , contractDemos
        , currentDemoName: Nothing
        , compilationResult: NotAsked
        , simulatorState: Simulator.initialState
        , authStatus: NotAsked
        , createGistResult: NotAsked
        , gistUrl: Nothing
        , blockchainVisualisationState: Chain.initialState
        }

------------------------------------------------------------
ajaxSettings :: SPSettings_ SPParams_
ajaxSettings = defaultSettings $ SPParams_ { baseURL: "/api/" }

mkMainFrame ::
  forall m n.
  MonadThrow Error n =>
  MonadEffect n =>
  MonadAff m =>
  n (Component HTML Query HAction Void m)
mkMainFrame = do
  editorState <- Editor.initialState
  initialState <- mkInitialState editorState
  pure $ hoist (flip runReaderT ajaxSettings)
    $ H.mkComponent
        { initialState: const initialState
        , render
        , eval:
            H.mkEval
              { handleAction: handleActionWithAnalyticsTracking
              , handleQuery: const $ pure Nothing
              , initialize: Just Init
              , receive: const Nothing
              , finalize: Nothing
              }
        }

-- TODO: use web-common withAnalytics function
handleActionWithAnalyticsTracking ::
  forall m.
  MonadEffect m =>
  MonadAsk (SPSettings_ SPParams_) m =>
  MonadAff m =>
  HAction -> HalogenM State HAction ChildSlots Void m Unit
handleActionWithAnalyticsTracking action = do
  liftEffect $ analyticsTracking action
  runHalogenApp $ handleAction action

handleAction ::
  forall m.
  MonadState State m =>
  MonadClipboard m =>
  MonadAsk (SPSettings_ SPParams_) m =>
  MonadApp m =>
  MonadAnimate m State =>
  HAction -> m Unit
handleAction Init = do
  handleAction CheckAuthStatus
  editorHandleAction $ Editor.Init

handleAction Mounted = pure unit

-- We just ignore most Chartist events.
handleAction (HandleBalancesChartMessage _) = pure unit

handleAction CheckAuthStatus = do
  assign _authStatus Loading
  authResult <- getOauthStatus
  assign _authStatus authResult

handleAction (GistAction subEvent) = handleGistAction subEvent

handleAction ToggleDemoFilesMenu = modifying _demoFilesMenuVisible not

handleAction (LoadScript key) = do
  contractDemos <- use _contractDemos
  case lookupContractDemo key contractDemos of
    Nothing -> pure unit
    Just (ContractDemo { contractDemoName, contractDemoEditorContents, contractDemoSimulations, contractDemoContext }) -> do
      editorSetContents contractDemoEditorContents (Just 1)
      saveBuffer (unwrap contractDemoEditorContents)
      assign _demoFilesMenuVisible false
      assign _currentView Editor
      assign _currentDemoName (Just contractDemoName)
      assign (_simulatorState <<< _simulations) $ Cursor.fromArray contractDemoSimulations
      assign (_editorState <<< _lastCompiledCode) (Just contractDemoEditorContents)
      assign (_editorState <<< _currentCodeIsCompiled) true
      assign _compilationResult (Success <<< Right $ contractDemoContext)
      assign (_simulatorState <<< _evaluationResult) NotAsked
      assign _createGistResult NotAsked

handleAction (ChangeView view) = do
  assign _currentView view
  when (view == Editor) resizeEditor
  when (view == Simulator) resizeBalancesChart

handleAction (EditorAction action) = editorHandleAction action

handleAction CompileProgram = do
  mContents <- editorGetContents
  case mContents of
    Nothing -> pure unit
    Just contents -> do
      oldCompilationResult <- use _compilationResult
      assign _compilationResult Loading
      newCompilationResult <- postContract contents
      assign _compilationResult newCompilationResult
      -- If we got a successful result, update lastCompiledCode and switch tab.
      case newCompilationResult of
        Success (Left _) -> assign (_editorState <<< _feedbackPaneMinimised) false
        _ ->
          when (isSuccess newCompilationResult) do
            assign (_editorState <<< _lastCompiledCode) (Just contents)
            assign (_editorState <<< _currentCodeIsCompiled) true
      -- Update the error display.
      editorSetAnnotations
        $ case newCompilationResult of
            Success (Left errors) -> toAnnotations errors
            _ -> []
      -- If we have a result with new signatures, we can only hold
      -- onto the old actions if the signatures still match. Any
      -- change means we'll have to clear out the existing simulation.
      -- Same thing for currencies.
      -- Potentially we could be smarter about this. But for now,
      -- let's at least be correct.
      let
        oldSignatures = preview (_details <<< _functionSchema) oldCompilationResult

        newSignatures = preview (_details <<< _functionSchema) newCompilationResult

        oldCurrencies = preview (_details <<< _knownCurrencies) oldCompilationResult

        newCurrencies = preview (_details <<< _knownCurrencies) newCompilationResult
      unless
        ( oldSignatures == newSignatures
            && oldCurrencies
            == newCurrencies
        )
        ( assign (_simulatorState <<< _simulations)
            $ case newCurrencies of
                Just currencies -> Cursor.singleton $ Simulator.mkSimulation currencies 1
                Nothing -> Cursor.empty
        )
      pure unit

handleAction (SimulatorAction simulatorAction) = do
  knownCurrencies <- getKnownCurrencies
  let
    input = Simulator.Input { knownCurrencies }
  Simulator.handleAction input simulatorAction

handleAction EvaluateActions =
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
              assign _blockchainVisualisationState Chain.initialState
              -- preselect the first transaction (if any)
              mAnnotatedBlockchain <- peruse (_simulatorState <<< _successfulEvaluationResult <<< _resultRollup <<< to AnnotatedBlockchain)
              txId <- (gets <<< lastOf) (_simulatorState <<< _successfulEvaluationResult <<< _resultRollup <<< traversed <<< traversed <<< _txIdOf)
              lift $ zoomStateT _blockchainVisualisationState $ Chain.handleAction (FocusTx txId) mAnnotatedBlockchain
            -- FIXME replaceViewOnSuccess result Simulations Transactions
            lift $ scrollIntoView simulatorTitleRefLabel
          Success (Left _) -> do
            -- on failed evaluation, scroll the error pane into view
            lift $ scrollIntoView simulationsErrorRefLabel
          Failure _ -> do
            -- on failed response, scroll the ajax error pane into view
            lift $ scrollIntoView ajaxErrorRefLabel
          _ -> pure unit
        pure unit

handleAction (ChainAction subaction) = do
  mAnnotatedBlockchain <-
    peruse (_simulatorState <<< _successfulEvaluationResult <<< _resultRollup <<< to AnnotatedBlockchain)
  let
    wrapper = case subaction of
      (FocusTx _) -> animate (_blockchainVisualisationState <<< _chainFocusAppearing)
      _ -> identity
  wrapper
    $ zoomStateT _blockchainVisualisationState
    $ Chain.handleAction subaction mAnnotatedBlockchain

_details :: forall a. Traversal' (WebData (Either InterpreterError (InterpreterResult a))) a
_details = _Success <<< _Right <<< _InterpreterResult <<< _result

handleGistAction :: forall m. MonadApp m => MonadState State m => GistAction -> m Unit
handleGistAction PublishOrUpdateGist = do
  void
    $ runMaybeT do
        mContents <- lift $ editorGetContents
        simulations <- use (_simulatorState <<< _simulations)
        newGist <- hoistMaybe $ mkNewGist { source: mContents, simulations }
        mGist <- use _createGistResult
        assign _createGistResult Loading
        newResult <-
          lift
            $ case preview (_Success <<< gistId) mGist of
                Nothing -> postGist newGist
                Just existingGistId -> postGistByGistId newGist existingGistId
        assign _createGistResult newResult
        gistId <- hoistMaybe $ preview (_Success <<< gistId <<< _GistId) newResult
        assign _gistUrl (Just gistId)
        when (isSuccess newResult) do
          assign _currentView Editor
          assign _currentDemoName Nothing

handleGistAction (SetGistUrl newGistUrl) = assign _gistUrl (Just newGistUrl)

handleGistAction LoadGist =
  void $ runExceptT
    $ do
        mGistId <- ExceptT (note "Gist Url not set." <$> use _gistUrl)
        eGistId <- except $ Gists.parseGistUrl mGistId
        --
        assign _createGistResult Loading
        assign _gistErrorPaneVisible true
        aGist <- lift $ getGistByGistId eGistId
        assign _createGistResult aGist
        when (isSuccess aGist) do
          assign _currentView Editor
          assign _currentDemoName Nothing
        gist <- ExceptT $ pure $ toEither (Left "Gist not loaded.") $ lmap errorToString aGist
        --
        -- Load the source, if available.
        content <- noteT "Source not found in gist." $ view playgroundGistFile gist
        lift $ editorSetContents (SourceCode content) (Just 1)
        lift $ saveBuffer content
        assign (_simulatorState <<< _simulations) Cursor.empty
        assign (_simulatorState <<< _evaluationResult) NotAsked
        --
        -- Load the simulation, if available.
        simulationString <- noteT "Simulation not found in gist." $ view simulationGistFile gist
        simulations <- mapExceptT (pure <<< unwrap) $ withExceptT renderForeignErrors $ decodeJSON simulationString
        assign (_simulatorState <<< _simulations) simulations
  where
  toEither :: forall e a. Either e a -> RemoteData e a -> Either e a
  toEither _ (Success a) = Right a

  toEither _ (Failure e) = Left e

  toEither x Loading = x

  toEither x NotAsked = x

handleGistAction (AjaxErrorPaneAction CloseErrorPane) = assign _gistErrorPaneVisible false

replaceViewOnSuccess :: forall m e a. MonadState State m => RemoteData e a -> View -> View -> m Unit
replaceViewOnSuccess result source target = do
  currentView <- use _currentView
  when (isSuccess result && currentView == source)
    (assign _currentView target)

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

toAnnotations :: InterpreterError -> Array IMarkerData
toAnnotations (TimeoutError _) = []

toAnnotations (CompilationErrors errors) = catMaybes (toAnnotation <$> errors)

toAnnotation :: CompilationError -> Maybe IMarkerData
toAnnotation (RawError _) = Nothing

toAnnotation (CompilationError { row, column, text }) =
  Just
    { severity: markerSeverity "Error"
    , message: String.joinWith "\\n" text
    , startLineNumber: row
    , startColumn: column
    , endLineNumber: row
    , endColumn: column
    , code: mempty
    , source: mempty
    }
