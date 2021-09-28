module Simulator.Types
  ( State(..)
  , Input(..)
  , Action(..)
  , SimulatorAction(..)
  , DragAndDropEventType(..)
  , WalletEvent(..)
  ) where

import Analytics (class IsEvent, defaultEvent)
import Cursor (Cursor)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Playground.Types (ContractCall, KnownCurrency, Simulation)
import Prelude (class Show, show, ($))
import Schema.Types (ActionEvent(..), FormArgument, SimulationAction(..))
import Types (WebEvaluationResult)
import ValueEditor (ValueEvent(..))
import Web.HTML.Event.DragEvent (DragEvent)

newtype State
  = State
  { simulations :: Cursor Simulation
  , actionDrag :: Maybe Int
  , evaluationResult :: WebEvaluationResult
  , lastEvaluatedSimulation :: Maybe Simulation
  , transactionsOpen :: Boolean
  }

derive instance newtypeState :: Newtype State _

newtype Input
  = Input
  { knownCurrencies :: Array KnownCurrency
  }

data Action
  = AddSimulationSlot
  | SetSimulationSlot Int
  | RemoveSimulationSlot Int
  | ActionDragAndDrop Int DragAndDropEventType DragEvent
  | ModifyWallets WalletEvent
  | ChangeSimulation SimulationAction
  | SetTransactionsOpen Boolean

-- this synonym is defined in playground-common/src/Playground/Types.hs
type SimulatorAction
  = ContractCall FormArgument

data DragAndDropEventType
  = DragStart
  | DragEnd
  | DragEnter
  | DragOver
  | DragLeave
  | Drop

instance showDragAndDropEventType :: Show DragAndDropEventType where
  show DragStart = "DragStart"
  show DragEnd = "DragEnd"
  show DragEnter = "DragEnter"
  show DragOver = "DragOver"
  show DragLeave = "DragLeave"
  show Drop = "Drop"

data WalletEvent
  = AddWallet
  | RemoveWallet Int
  | ModifyBalance Int ValueEvent

-- | Here we decide which top-level queries to track as GA events, and
-- how to classify them.
instance actionIsEvent :: IsEvent Action where
  toEvent AddSimulationSlot = Just $ (defaultEvent "AddSimulationSlot") { category = Just "Simulation" }
  toEvent (SetSimulationSlot _) = Just $ (defaultEvent "SetSimulationSlot") { category = Just "Simulation" }
  toEvent (RemoveSimulationSlot _) = Just $ (defaultEvent "RemoveSimulationSlot") { category = Just "Simulation" }
  toEvent (ModifyWallets AddWallet) = Just $ (defaultEvent "AddWallet") { category = Just "Wallet" }
  toEvent (ModifyWallets (RemoveWallet _)) = Just $ (defaultEvent "RemoveWallet") { category = Just "Wallet" }
  toEvent (ModifyWallets (ModifyBalance _ (SetBalance _ _ _))) = Just $ (defaultEvent "SetBalance") { category = Just "Wallet" }
  toEvent (ActionDragAndDrop _ eventType _) = Just $ (defaultEvent (show eventType)) { category = Just "Action" }
  toEvent (ChangeSimulation (PopulateAction _ _)) = Just $ (defaultEvent "PopulateAction") { category = Just "Action" }
  toEvent (ChangeSimulation (ModifyActions (AddAction _))) = Just $ (defaultEvent "AddAction") { category = Just "Action" }
  toEvent (ChangeSimulation (ModifyActions (AddWaitAction _))) = Just $ (defaultEvent "AddWaitAction") { category = Just "Action" }
  toEvent (ChangeSimulation (ModifyActions (RemoveAction _))) = Just $ (defaultEvent "RemoveAction") { category = Just "Action" }
  toEvent (ChangeSimulation (ModifyActions (SetPayToWalletValue _ _))) = Just $ (defaultEvent "SetPayToWalletValue") { category = Just "Action" }
  toEvent (ChangeSimulation (ModifyActions (SetPayToWalletRecipient _ _))) = Just $ (defaultEvent "SetPayToWalletRecipient") { category = Just "Action" }
  toEvent (ChangeSimulation (ModifyActions (SetWaitTime _ _))) = Just $ (defaultEvent "SetWaitTime") { category = Just "Action" }
  toEvent (ChangeSimulation (ModifyActions (SetWaitUntilTime _ _))) = Just $ (defaultEvent "SetWaitUntilTime") { category = Just "Action" }
  toEvent (SetTransactionsOpen true) = Just $ defaultEvent "OpenTransactions"
  toEvent (SetTransactionsOpen false) = Just $ defaultEvent "CloseTransactions"
