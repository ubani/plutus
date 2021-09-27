module Simulator.Types
  ( State(..)
  ) where

import Cursor (Cursor)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Playground.Types (Simulation)
import Types (WebEvaluationResult)

newtype State
  = State
  { simulations :: Cursor Simulation
  , actionDrag :: Maybe Int
  , evaluationResult :: WebEvaluationResult
  , lastEvaluatedSimulation :: Maybe Simulation
  }

derive instance newtypeState :: Newtype State _
