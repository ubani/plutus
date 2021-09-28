module Component.Wallets.Types.Internal where

import Prologue
import AppM (AppM)
import Capability.Marlowe (class ManageMarlowe)
import Capability.MarloweStorage (class ManageMarloweStorage)
import Capability.Toast (class Toast)
import Clipboard (class MonadClipboard)
import Component.Wallets.Types (Input, Msg)
import Contacts.Types (WalletDetails, WalletLibrary)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested (type (/\))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import InputField.Types (class InputFieldError, Action, State) as InputField
import Marlowe.PAB (PlutusAppId)
import Types (WebData)

data Action
  = Init
  | Receive Input
  | GenerateWallet
  | ExplainWalletGeneration
  | WatchGetStarted
  | SearchKeyAction (InputField.Action SearchKeyError)
  | ConnectWallet WalletDetails
  | CloseModal
  | ClearCache
  | CopyWalletId PlutusAppId

type State
  = { modal :: Maybe (Boolean /\ Modal)
    , searchKey :: InputField.State SearchKeyError
    , walletLibrary :: WalletLibrary
    , walletDetails :: WebData WalletDetails
    , connecting :: Boolean
    }

_modal :: Lens' State (Maybe (Boolean /\ Modal))
_modal = prop (SProxy :: _ "modal")

_searchKey :: Lens' State (InputField.State SearchKeyError)
_searchKey = prop (SProxy :: _ "searchKey")

_walletLibrary :: Lens' State (WalletLibrary)
_walletLibrary = prop (SProxy :: _ "walletLibrary")

_walletDetails :: Lens' State (WebData WalletDetails)
_walletDetails = prop (SProxy :: _ "walletDetails")

_connecting :: Lens' State (Boolean)
_connecting = prop (SProxy :: _ "connecting")

data SearchKeyError
  = UnconfirmedSearchKey
  | NonexistentSearchKey

derive instance eqSearchKeyError :: Eq SearchKeyError

instance inputFieldErrorSearchKeyError ::
  InputField.InputFieldError SearchKeyError where
  inputErrorToString UnconfirmedSearchKey = "Looking up wallet..."
  inputErrorToString NonexistentSearchKey = "Wallet not found"

data Modal
  = GetStartedHelp
  | GenerateWalletHelp
  | UseNewWallet WalletDetails
  | UseWallet WalletDetails
  | LocalWalletMissing

type Slots
  = ( useNewWalletSlot :: forall q. H.Slot q Action Unit
    )

type DSL
  = H.HalogenM State Action Slots Msg

type ComponentHTML m
  = H.ComponentHTML Action Slots m

class
  ( MonadAff m
  , MonadClipboard m
  , ManageMarlowe m
  , ManageMarloweStorage m
  , Toast m
  ) <= MonadHandler m

instance monadHandlerAppM :: MonadHandler AppM
