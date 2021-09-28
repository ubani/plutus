module MainFrame.Types
  ( State(..)
  , View(..)
  , ChainSlot
  , Blockchain
  , Query
  , HAction(..)
  , ChildSlots
  ) where

import Analytics (class IsEvent, defaultEvent, toEvent)
import Auth (AuthStatus)
import Chain.Types (Action(..))
import Chain.Types as Chain
import Clipboard as Clipboard
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.NonEmpty ((:|))
import Editor.Types (Action(..), State) as Editor
import Gist (Gist)
import Gists.Types (GistAction(..))
import Halogen as H
import Halogen.Chartist as Chartist
import Halogen.Monaco as Monaco
import Playground.Types (ContractDemo)
import Plutus.V1.Ledger.Tx (Tx)
import Prelude (class Eq, class Show, Unit, show, ($))
import Simulator.Types (Action, State) as Simulator
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen as Gen
import Types (WebCompilationResult, WebData)

newtype State
  = State
  { demoFilesMenuVisible :: Boolean
  , gistErrorPaneVisible :: Boolean
  , currentView :: View
  , contractDemos :: Array ContractDemo
  , currentDemoName :: Maybe String
  , editorState :: Editor.State
  , compilationResult :: WebCompilationResult
  , simulatorState :: Simulator.State
  , authStatus :: WebData AuthStatus
  , createGistResult :: WebData Gist
  , gistUrl :: Maybe String
  , blockchainVisualisationState :: Chain.State
  }

derive instance newtypeState :: Newtype State _

data View
  = Editor
  | Simulator

derive instance eqView :: Eq View

derive instance genericView :: Generic View _

instance arbitraryView :: Arbitrary View where
  arbitrary = Gen.elements (Editor :| [ Simulator ])

instance showView :: Show View where
  show Editor = "Editor"
  show Simulator = "Simulator"

type ChainSlot
  = Array Tx

type Blockchain
  = Array ChainSlot

data Query a

data HAction
  = Init
  | Mounted
  | HandleBalancesChartMessage Chartist.Message
  | CheckAuthStatus
  | GistAction GistAction
  | ToggleDemoFilesMenu
  | LoadScript String
  | ChangeView View
  | EditorAction Editor.Action
  | CompileProgram
  | SimulatorAction Simulator.Action
  | EvaluateActions
  | ChainAction (Chain.Action)

type ChildSlots
  = ( editorSlot :: H.Slot Monaco.Query Monaco.Message Unit
    , balancesChartSlot :: H.Slot Chartist.Query Chartist.Message Unit
    )

-- | Here we decide which top-level queries to track as GA events, and
-- how to classify them.
instance actionIsEvent :: IsEvent HAction where
  toEvent Init = Nothing
  toEvent Mounted = Just $ defaultEvent "Mounted"
  toEvent (EditorAction (Editor.HandleDropEvent _)) = Just $ defaultEvent "DropScript"
  toEvent (EditorAction action) = Just $ defaultEvent "ConfigureEditor"
  toEvent CompileProgram = Just $ defaultEvent "CompileProgram"
  toEvent (HandleBalancesChartMessage _) = Nothing
  toEvent CheckAuthStatus = Nothing
  toEvent (GistAction PublishOrUpdateGist) = Just $ (defaultEvent "Publish") { category = Just "Gist" }
  toEvent (GistAction (SetGistUrl _)) = Nothing
  toEvent (GistAction LoadGist) = Just $ (defaultEvent "LoadGist") { category = Just "Gist" }
  toEvent (GistAction (AjaxErrorPaneAction _)) = Nothing
  toEvent ToggleDemoFilesMenu = Nothing
  toEvent (ChangeView view) = Just $ (defaultEvent "View") { label = Just $ show view }
  toEvent (LoadScript script) = Just $ (defaultEvent "LoadScript") { label = Just script }
  toEvent (SimulatorAction action) = toEvent action
  toEvent EvaluateActions = Just $ (defaultEvent "EvaluateActions") { category = Just "Action" }
  toEvent (ChainAction (FocusTx (Just _))) = Just $ (defaultEvent "BlockchainFocus") { category = Just "Transaction" }
  toEvent (ChainAction (FocusTx Nothing)) = Nothing
  toEvent (ChainAction (ClipboardAction (Clipboard.CopyToClipboard _))) = Just $ (defaultEvent "ClipboardAction") { category = Just "CopyToClipboard" }
