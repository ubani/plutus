{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Plutus.Contract.Effects( -- TODO: Move to Requests.Internal
    PABReq(..),
    _AwaitSlotReq,
    _AwaitTimeReq,
    _CurrentSlotReq,
    _CurrentTimeReq,
    _AwaitTxStatusChangeReq,
    _OwnContractInstanceIdReq,
    _OwnPublicKeyReq,
    _UtxoAtReq,
    _AwaitUtxoSpentReq,
    _AwaitUtxoProducedReq,
    _BalanceTxReq,
    _WriteBalancedTxReq,
    _ExposeEndpointReq,
    PABResp(..),
    _AwaitSlotResp,
    _AwaitTimeResp,
    _CurrentSlotResp,
    _CurrentTimeResp,
    _AwaitTxStatusChangeResp,
    _OwnContractInstanceIdResp,
    _OwnPublicKeyResp,
    _UtxoAtResp,
    _AwaitUtxoSpentResp,
    _AwaitUtxoProducedResp,
    _BalanceTxResp,
    _WriteBalancedTxResp,
    _ExposeEndpointResp,
    matches,

    -- * Etc.
    UtxoAtAddress(..),
    BalanceTxResponse(..),
    balanceTxResponse,
    WriteBalancedTxResponse(..),
    writeBalancedTxResponse,
    ActiveEndpoint(..),
    TxConfirmed(..),
    TxStatus(..)
    ) where

import           Control.Lens                (Iso', iso, makePrisms)
import           Data.Aeson                  (FromJSON, ToJSON)
import qualified Data.Aeson                  as JSON
import qualified Data.Map                    as Map
import           Data.Text.Prettyprint.Doc   (Pretty (..), colon, indent, parens, viaShow, vsep, (<+>))
import           GHC.Generics                (Generic)
import           Ledger                      (Address, OnChainTx, PubKey, Tx, TxId, TxOutRef, TxOutTx (..), txId)
import           Ledger.AddressMap           (UtxoMap)
import           Ledger.Constraints.OffChain (UnbalancedTx)
import           Ledger.Slot                 (Slot (..))
import           Ledger.Time                 (POSIXTime (..))
import           Wallet.API                  (WalletAPIError)
import           Wallet.Types                (ContractInstanceId, EndpointDescription, EndpointValue)

-- | Requests that 'Contract's can make
data PABReq =
    AwaitSlotReq Slot
    | AwaitTimeReq POSIXTime
    | AwaitUtxoSpentReq TxOutRef
    | AwaitUtxoProducedReq Address
    | AwaitTxStatusChangeReq TxId
    | CurrentSlotReq
    | CurrentTimeReq
    | OwnContractInstanceIdReq
    | OwnPublicKeyReq
    | UtxoAtReq Address
    | BalanceTxReq UnbalancedTx
    | WriteBalancedTxReq Tx
    | ExposeEndpointReq ActiveEndpoint
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty PABReq where
  pretty = \case
    AwaitSlotReq s              -> "Await slot:" <+> pretty s
    AwaitTimeReq s              -> "Await time:" <+> pretty s
    AwaitUtxoSpentReq utxo      -> "Await utxo spent:" <+> pretty utxo
    AwaitUtxoProducedReq a      -> "Await utxo produced:" <+> pretty a
    AwaitTxStatusChangeReq txid -> "Await tx status change:" <+> pretty txid
    CurrentSlotReq              -> "Current slot"
    CurrentTimeReq              -> "Current time"
    OwnContractInstanceIdReq    -> "Own contract instance ID"
    OwnPublicKeyReq             -> "Own public key"
    UtxoAtReq addr              -> "Utxo at:" <+> pretty addr
    BalanceTxReq utx            -> "Balance tx:" <+> pretty utx
    WriteBalancedTxReq tx       -> "Write balanced tx:" <+> pretty tx
    ExposeEndpointReq ep        -> "Expose endpoint:" <+> pretty ep

-- | Responses that 'Contract's receive
data PABResp =
    AwaitSlotResp Slot
    | AwaitTimeResp POSIXTime
    | AwaitUtxoSpentResp OnChainTx
    | AwaitUtxoProducedResp [OnChainTx]
    | AwaitTxStatusChangeResp TxStatus
    | CurrentSlotResp Slot
    | CurrentTimeResp POSIXTime
    | OwnContractInstanceIdResp ContractInstanceId
    | OwnPublicKeyResp PubKey
    | UtxoAtResp UtxoAtAddress
    | BalanceTxResp BalanceTxResponse
    | WriteBalancedTxResp WriteBalancedTxResponse
    | ExposeEndpointResp EndpointDescription (EndpointValue JSON.Value)
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)


instance Pretty PABResp where
  pretty = \case
    AwaitSlotResp s              -> "Slot:" <+> pretty s
    AwaitTimeResp s              -> "Time:" <+> pretty s
    AwaitUtxoSpentResp utxo      -> "Utxo spent:" <+> pretty utxo
    AwaitUtxoProducedResp addr   -> "Utxo produced:" <+> pretty addr
    AwaitTxStatusChangeResp txid -> "Tx confirmed:" <+> pretty txid
    CurrentSlotResp s            -> "Current slot:" <+> pretty s
    CurrentTimeResp s            -> "Current time:" <+> pretty s
    OwnContractInstanceIdResp i  -> "Own contract instance ID:" <+> pretty i
    OwnPublicKeyResp k           -> "Own public key:" <+> pretty k
    UtxoAtResp rsp               -> "Utxo at:" <+> pretty rsp
    BalanceTxResp r              -> "Balance tx:" <+> pretty r
    WriteBalancedTxResp r        -> "Write balanced tx:" <+> pretty r
    ExposeEndpointResp desc rsp  -> "Call endpoint" <+> pretty desc <+> "with" <+> pretty rsp

matches :: PABReq -> PABResp -> Bool
matches a b = case (a, b) of
  (AwaitSlotReq{}, AwaitSlotResp{})                       -> True
  (AwaitTimeReq{}, AwaitTimeResp{})                       -> True
  (AwaitUtxoSpentReq{}, AwaitUtxoSpentResp{})                       -> True
  (AwaitUtxoProducedReq{}, AwaitUtxoProducedResp{})                -> True
  (AwaitTxStatusChangeReq{}, AwaitTxStatusChangeResp{})         -> True
  (CurrentSlotReq, CurrentSlotResp{})                     -> True
  (CurrentTimeReq, CurrentTimeResp{})                     -> True
  (OwnContractInstanceIdReq, OwnContractInstanceIdResp{}) -> True
  (OwnPublicKeyReq, OwnPublicKeyResp{})                   -> True
  (UtxoAtReq{}, UtxoAtResp{})                             -> True
  (BalanceTxReq{}, BalanceTxResp{})                       -> True
  (WriteBalancedTxReq{}, WriteBalancedTxResp{})           -> True
  (ExposeEndpointReq ActiveEndpoint{aeDescription}, ExposeEndpointResp desc _)
    | aeDescription == desc -> True
  _                                                       -> False

data UtxoAtAddress =
    UtxoAtAddress
        { address :: Address
        , utxo    :: UtxoMap
        }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty UtxoAtAddress where
  pretty UtxoAtAddress{address, utxo} =
    let
      prettyTxOutPair (txoutref, TxOutTx{txOutTxOut}) =
        pretty txoutref <> colon <+> pretty txOutTxOut
      utxos = vsep $ fmap prettyTxOutPair (Map.toList utxo)
    in vsep ["Utxo at" <+> pretty address <+> "=", indent 2 utxos]

-- | The status of a Cardano transaction
data TxStatus =
  OnChain Int -- ^ The transaction is on the chain, n blocks deep. It can still be rolled back.
  | Committed -- ^ The transaction is on the chain. It cannot rolled back anymore.
  | Unknown -- ^ The transaction is not on the chain. That's all we can say.
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Pretty TxStatus where
  pretty = \case
    OnChain i -> "OnChain" <+> parens (pretty i <+> "blocks deep")
    e         -> viaShow e

data BalanceTxResponse =
  BalanceTxFailed WalletAPIError
  | BalanceTxSuccess Tx
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Pretty BalanceTxResponse where
  pretty = \case
    BalanceTxFailed e  -> "BalanceTxFailed:" <+> pretty e
    BalanceTxSuccess i -> "BalanceTxSuccess:" <+> pretty (txId i)

balanceTxResponse :: Iso' BalanceTxResponse (Either WalletAPIError Tx)
balanceTxResponse = iso f g where
  f = \case { BalanceTxFailed w -> Left w; BalanceTxSuccess t -> Right t }
  g = either BalanceTxFailed BalanceTxSuccess

data WriteBalancedTxResponse =
  WriteBalancedTxFailed WalletAPIError
  | WriteBalancedTxSuccess Tx
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Pretty WriteBalancedTxResponse where
  pretty = \case
    WriteBalancedTxFailed e  -> "WriteBalancedTxFailed:" <+> pretty e
    WriteBalancedTxSuccess i -> "WriteBalancedTxSuccess:" <+> pretty (txId i)

writeBalancedTxResponse :: Iso' WriteBalancedTxResponse (Either WalletAPIError Tx)
writeBalancedTxResponse = iso f g where
  f = \case { WriteBalancedTxFailed w -> Left w; WriteBalancedTxSuccess t -> Right t }
  g = either WriteBalancedTxFailed WriteBalancedTxSuccess

data ActiveEndpoint = ActiveEndpoint
  { aeDescription :: EndpointDescription -- ^ The name of the endpoint
  , aeMetadata    :: Maybe JSON.Value -- ^ Data that should be shown to the user
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Pretty ActiveEndpoint where
  pretty ActiveEndpoint{aeDescription, aeMetadata} =
    indent 2 $ vsep
      [ "Endpoint:" <+> pretty aeDescription
      , "Metadata:" <+> viaShow aeMetadata
      ]

newtype TxConfirmed =
    TxConfirmed { unTxConfirmed :: TxId }
        deriving stock (Eq, Ord, Generic, Show)
        deriving anyclass (ToJSON, FromJSON)
        deriving Pretty via TxId

makePrisms ''PABReq

makePrisms ''PABResp
