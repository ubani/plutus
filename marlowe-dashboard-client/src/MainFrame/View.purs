module MainFrame.View where

import Prologue hiding (div)
import Component.Wallets (walletsScreen)
import Component.Wallets.Types.Internal (class MonadHandler)
import Dashboard.View (dashboardCard, dashboardScreen)
import Data.Lens (view, (^.))
import Halogen (ComponentHTML)
import Halogen.Css (classNames)
import Halogen.Extra (renderSubmodule)
import Halogen.HTML (div)
import MainFrame.Lenses (_connecting, _currentSlot, _dashboardState, _subState, _toast, _tzOffset, _welcomeState)
import MainFrame.Types (Action(..), ChildSlots, State)
import Toast.View (renderToast)
import Welcome.View (welcomeCard)

render :: forall m. MonadHandler m => State -> ComponentHTML Action ChildSlots m
render state =
  let
    currentSlot = state ^. _currentSlot

    tzOffset = state ^. _tzOffset

    connecting = state ^. _connecting
  in
    div [ classNames [ "h-full" ] ]
      $ case view _subState state of
          Left _ ->
            [ walletsScreen connecting WalletsMsg
            , renderSubmodule _welcomeState WelcomeAction welcomeCard state
            ]
          Right _ ->
            [ renderSubmodule _dashboardState DashboardAction (dashboardScreen { currentSlot, tzOffset }) state
            , renderSubmodule _dashboardState DashboardAction (dashboardCard currentSlot) state
            ]
      <> [ renderSubmodule _toast ToastAction renderToast state ]
