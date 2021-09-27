module Route where

import Prologue
import Data.Generic.Rep (class Generic)
import Marlowe.PAB (PlutusAppId)

data Route
  = Wallets
  | Wallet PlutusAppId WalletRoute

derive instance genericRoute :: Generic Route _

derive instance eqRoute :: Eq Route

derive instance ordRoute :: Ord Route

data WalletRoute
  = Contracts
  | Contract PlutusAppId

derive instance genericWalletRoute :: Generic WalletRoute _

derive instance eqWalletRoute :: Eq WalletRoute

derive instance ordWalletRoute :: Ord WalletRoute
