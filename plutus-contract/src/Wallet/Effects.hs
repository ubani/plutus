{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
module Wallet.Effects(
    WalletEffects
    -- * Wallet effect
    , WalletEffect(..)
    , submitTxn
    , ownPubKey
    , balanceTx
    , totalFunds
    , walletAddSignature
    -- * Node client
    , NodeClientEffect(..)
    , publishTx
    , getClientSlot
    -- * Chain index
    , ChainIndexEffect(..)
    , startWatching
    , watchedAddresses
    , confirmedBlocks
    -- * Contract runtime
    , ContractRuntimeEffect(..)
    , sendNotification
    ) where

import           Control.Monad.Freer.TH      (makeEffect)
import           Ledger                      (Address, Block, PubKey, Slot, Tx, Value)
import           Ledger.AddressMap           (AddressMap)
import           Ledger.Constraints.OffChain (UnbalancedTx)
import           Wallet.Emulator.Error       (WalletAPIError)
import           Wallet.Types                (Notification, NotificationError)

data WalletEffect r where
    SubmitTxn :: Tx -> WalletEffect ()
    OwnPubKey :: WalletEffect PubKey
    BalanceTx :: UnbalancedTx -> WalletEffect (Either WalletAPIError Tx)
    TotalFunds :: WalletEffect Value -- ^ Total of all funds that are in the wallet (incl. tokens)
    WalletAddSignature :: Tx -> WalletEffect Tx
makeEffect ''WalletEffect

data NodeClientEffect r where
    PublishTx :: Tx -> NodeClientEffect ()
    GetClientSlot :: NodeClientEffect Slot
makeEffect ''NodeClientEffect

{-| Access the chain index. The chain index keeps track of the
    datums that are associated with unspent transaction outputs. Addresses that
    are of interest need to be added with 'startWatching' before their outputs
    show up in the 'AddressMap' returned by 'watchedAddresses'.
-}
-- TODO: This needs to go (replaced by Plutus.ChainIndex.Effects.ChainIndexQueryEffect)
data ChainIndexEffect r where
    StartWatching :: Address -> ChainIndexEffect ()
    WatchedAddresses :: ChainIndexEffect AddressMap
    ConfirmedBlocks :: ChainIndexEffect [Block]
makeEffect ''ChainIndexEffect

{-| Interact with other contracts.
-}
data ContractRuntimeEffect r where
    SendNotification :: Notification -> ContractRuntimeEffect (Maybe NotificationError)

makeEffect ''ContractRuntimeEffect

-- | Effects that allow contracts to interact with the blockchain
type WalletEffects =
    '[ WalletEffect
    , NodeClientEffect
    , ChainIndexEffect
    , ContractRuntimeEffect
    ]
