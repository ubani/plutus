module Simulator.State
  ( initialState
  ) where

import Cursor (empty) as Cursor
import Data.Maybe (Maybe(..))
import Network.RemoteData (RemoteData(..))
import Simulator.Types (State(..))

initialState :: State
initialState =
  State
    { simulations: Cursor.empty
    , actionDrag: Nothing
    , evaluationResult: NotAsked
    , lastEvaluatedSimulation: Nothing
    }
