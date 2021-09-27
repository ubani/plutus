module Types
  ( WebData(..)
  , WebCompilationResult(..)
  , WebEvaluationResult(..)
  ) where

import Data.Either (Either)
import Language.Haskell.Interpreter (InterpreterError, InterpreterResult)
import Network.RemoteData (RemoteData)
import Playground.Types (CompilationResult, EvaluationResult, PlaygroundError, Simulation)
import Servant.PureScript.Ajax (AjaxError)

type WebData
  = RemoteData AjaxError

type WebCompilationResult
  = WebData (Either InterpreterError (InterpreterResult CompilationResult))

type WebEvaluationResult
  = WebData (Either PlaygroundError EvaluationResult)
