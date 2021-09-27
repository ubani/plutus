module Route where

import Prologue
import Control.Monad.Trans.Class (lift)
import Data.Generic.Rep (class Generic)
import Halogen (HalogenM)
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

class
  Monad m <= MonadNavigate m where
  navigate :: Route -> m Unit

instance monadNavigateHalogenM ::
  MonadNavigate m =>
  MonadNavigate (HalogenM state action slots msg m) where
  navigate = lift <<< navigate
