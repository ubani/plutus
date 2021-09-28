module Component.Wallets.State
  ( handleAction
  , receive
  , generateWallet
  , explainWalletGeneration
  , watchGetStarted
  ) where

import Prologue
import Capability.Marlowe (createWallet)
import Clipboard (class MonadClipboard, copy)
import Component.Wallets.Types (Input, Msg(..))
import Component.Wallets.Types.Internal (class MonadHandler, Action(..), DSL, Modal(..), SearchKeyError, _connecting, _modal, _searchKey, _walletDetails)
import Contacts.Types (WalletDetails)
import Data.Either (either)
import Data.Lens (Setter', _1, _Just, assign)
import Data.Newtype (unwrap)
import Data.UUID as UUID
import Halogen as H
import Halogen.Extra (mapSubmodule)
import InputField.State (handleAction) as InputField
import InputField.Types (Action) as InputField
import Marlowe.PAB (PlutusAppId)
import Network.RemoteData (RemoteData(..), fromEither)

-------------------------------------------------------------------------------
-- Handlers
-------------------------------------------------------------------------------
handleAction :: forall m. MonadHandler m => Action -> DSL m Unit
handleAction = case _ of
  Init -> pure unit
  Receive input -> receive input
  GenerateWallet -> generateWallet
  ExplainWalletGeneration -> explainWalletGeneration
  WatchGetStarted -> watchGetStarted
  SearchKeyAction action -> searchKeyAction action
  ConnectWallet details -> connectWallet details
  CloseModal -> closeModal
  ClearCache -> clearCache
  CopyWalletId id -> copyWalletId id

receive :: forall m. Input -> DSL m Unit
receive = assign _connecting

generateWallet :: forall m. MonadHandler m => DSL m Unit
generateWallet =
  withRemoteState
    _walletDetails
    createWallet
    (H.raise <<< RemoteCallFailed "Failed to generate wallet.")
    (showModal <<< UseNewWallet)

explainWalletGeneration :: forall m. DSL m Unit
explainWalletGeneration = showModal GenerateWalletHelp

watchGetStarted :: forall m. DSL m Unit
watchGetStarted = showModal GetStartedHelp

connectWallet :: forall m. WalletDetails -> DSL m Unit
connectWallet = H.raise <<< WalletChosen

closeModal :: forall m. DSL m Unit
closeModal = assign (_modal <<< _Just <<< _1) false

clearCache :: forall m. DSL m Unit
clearCache = H.raise CacheClearRequested

copyWalletId :: forall m. Monad m => MonadClipboard m => PlutusAppId -> DSL m Unit
copyWalletId = copy <<< UUID.toString <<< unwrap

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------
showModal :: forall m. Modal -> DSL m Unit
showModal = assign _modal <<< Just <<< Tuple true

searchKeyAction :: forall m. Functor m => InputField.Action SearchKeyError -> DSL m Unit
searchKeyAction = mapSubmodule _searchKey SearchKeyAction <<< InputField.handleAction

withRemoteState ::
  forall s m e a r action slots msg.
  Monad m =>
  Setter' s (RemoteData e a) ->
  m (Either e a) ->
  (e -> H.HalogenM s action slots msg m r) ->
  (a -> H.HalogenM s action slots msg m r) ->
  H.HalogenM s action slots msg m r
withRemoteState _remoteState remoteCall onError onSuccess = do
  assign _remoteState Loading
  result <- H.lift remoteCall
  assign _remoteState $ fromEither result
  either onError onSuccess result
