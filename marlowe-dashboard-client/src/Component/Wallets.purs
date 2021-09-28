module Component.Wallets
  ( component
  , walletsScreen
  , module Component.Wallets.Types
  ) where

import Prologue
import Component.Wallets.State (handleAction)
import Component.Wallets.Types (Component, Input, Msg(..), Query, Slot)
import Component.Wallets.Types.Internal (class MonadHandler, Action(..), State)
import Component.Wallets.View (render)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML (slot)
import InputField.State (mkInitialState) as InputField
import Network.RemoteData (RemoteData(..))

walletsScreenSlot :: SProxy "walletsScreenSlot"
walletsScreenSlot = SProxy

walletsScreen ::
  forall slots m action.
  MonadHandler m =>
  Input ->
  (Msg -> action) ->
  H.ComponentHTML action ( walletsScreenSlot :: Slot | slots ) m
walletsScreen input handleMsg =
  slot
    walletsScreenSlot
    unit
    component
    input
    (Just <<< handleMsg)

component :: forall m. MonadHandler m => Component m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction
            , initialize = Just Init
            , receive = Just <<< Receive
            }
    }

initialState :: Input -> State
initialState connecting =
  { modal: Nothing
  , searchKey: InputField.mkInitialState Nothing
  , walletLibrary: mempty
  , walletDetails: NotAsked
  , connecting
  }
