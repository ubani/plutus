-- | This module contains all the public API types for the component.
module Component.Wallets.Types where

import Prologue
import Contacts.Types (WalletDetails)
import Halogen as H
import Halogen.HTML as HH
import Servant.PureScript.Ajax (AjaxError)

data Query a

type Input
  = Boolean

data Msg
  = WalletChosen WalletDetails
  | RemoteCallFailed String AjaxError
  | CacheClearRequested

type Slot
  = H.Slot Query Msg Unit

type Component
  = H.Component HH.HTML Query Input Msg
