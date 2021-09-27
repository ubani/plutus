module Simulator.Lenses
  ( _simulations
  , _actionDrag
  , _evaluationResult
  , _successfulEvaluationResult
  , _lastEvaluatedSimulation
  ) where

import Cursor (Cursor)
import Data.Either (Either)
import Data.Lens (Lens', Traversal', _Right)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Network.RemoteData (_Success)
import Playground.Types (EvaluationResult, PlaygroundError, Simulation)
import Prelude ((<<<))
import Simulator.Types (State)
import Types (WebData)

_simulations :: Lens' State (Cursor Simulation)
_simulations = _Newtype <<< prop (SProxy :: SProxy "simulations")

_actionDrag :: Lens' State (Maybe Int)
_actionDrag = _Newtype <<< prop (SProxy :: SProxy "actionDrag")

_evaluationResult :: Lens' State (WebData (Either PlaygroundError EvaluationResult))
_evaluationResult = _Newtype <<< prop (SProxy :: SProxy "evaluationResult")

_successfulEvaluationResult :: Traversal' State EvaluationResult
_successfulEvaluationResult = _evaluationResult <<< _Success <<< _Right

_lastEvaluatedSimulation :: Lens' State (Maybe Simulation)
_lastEvaluatedSimulation = _Newtype <<< prop (SProxy :: SProxy "lastEvaluatedSimulation")
