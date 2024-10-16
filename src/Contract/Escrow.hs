{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -g -fplugin-opt PlutusTx.Plugin:coverage-all #-}

-- | A general-purpose escrow contract in Plutus
module Contract.Escrow (
  -- $escrow
  Escrow,
  EscrowError (..),
  AsEscrowError (..),
  EscrowParams (..),
  EscrowTarget (..),
  payToScriptTarget,
  payToPaymentPubKeyTarget,
  targetTotal,
  payRedeemRefund,
  typedValidator,

  -- * Actions
  pay,
  redeem,
  refund,
  badRefund,
  RedeemFailReason (..),
  RedeemSuccess (..),
  RefundSuccess (..),

  -- * Exposed for test endpoints
  Action (..),
  Datum,
  validate,

  -- * Coverage
  covIdx,
) where

import Control.Lens (makeClassyPrisms)
import Control.Monad (void)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.RWS.Class (asks)
import Data.Map qualified as Map

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import PlutusTx (ToData)
import PlutusTx qualified
import PlutusTx.Code (getCovIdx)
import PlutusTx.Coverage (CoverageIndex)
import PlutusTx.Prelude ()
import PlutusTx.Prelude qualified as PlutusTx
import PlutusTx.TH (loadFromFile)

import Cardano.Node.Emulator qualified as E
import Cardano.Node.Emulator.Internal.Node (
  SlotConfig,
  pSlotConfig,
  posixTimeRangeToContainedSlotRange,
 )
import Cardano.Node.Emulator.Test (testnet)
import Data.Maybe (fromJust)
import Ledger (POSIXTime, PaymentPubKeyHash (unPaymentPubKeyHash), TxId, getCardanoTxId)
import Ledger qualified
import Ledger.Address (toWitness)
import Ledger.Tx.CardanoAPI qualified as C
import Ledger.Typed.Scripts (validatorCardanoAddress)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Script.Utils.Scripts (ValidatorHash, datumHash)
import Plutus.Script.Utils.V2.Contexts (
  ScriptContext (ScriptContext, scriptContextTxInfo),
  TxInfo,
  scriptOutputsAt,
  txInfoValidRange,
  txSignedBy,
 )
import Plutus.Script.Utils.V2.Typed.Scripts qualified as V2
import Plutus.Script.Utils.Value (Value, geq, lt)
import PlutusLedgerApi.V1.Interval qualified as Interval
import PlutusLedgerApi.V2 (Datum (Datum))
import PlutusLedgerApi.V2.Contexts (valuePaidTo)
import PlutusLedgerApi.V2.Tx (OutputDatum (OutputDatum))

data RedeemFailReason = DeadlinePassed | NotEnoughFundsAtAddress
  deriving stock (Eq, Show)

data EscrowError
  = RedeemFailed RedeemFailReason
  | RefundFailed
  deriving stock (Show)

makeClassyPrisms ''EscrowError

{- $escrow
The escrow contract implements the exchange of value between multiple
parties. It is defined by a list of targets (public keys and script
addresses, each associated with a value). It works similar to the
crowdfunding contract in that the contributions can be made independently,
and the funds can be unlocked only by a transaction that pays the correct
amount to each target. A refund is possible if the outputs locked by the
contract have not been spent by the deadline. (Compared to the crowdfunding
contract, the refund policy is simpler because here because there is no
"collection period" during which the outputs may be spent after the deadline
has passed. This is because we're assuming that the participants in the
escrow contract will make their deposits as quickly as possible after
agreeing on a deal)
-}

{- | Defines where the money should go. Usually we have `d = Datum` (when
  defining `EscrowTarget` values in off-chain code). Sometimes we have
  `d = DatumHash` (when checking the hashes in on-chain code)
-}
data EscrowTarget d
  = PaymentPubKeyTarget PaymentPubKeyHash Value
  | ScriptTarget ValidatorHash d Value
  deriving (Functor)

PlutusTx.makeLift ''EscrowTarget

-- | An 'EscrowTarget' that pays the value to a public key address.
payToPaymentPubKeyTarget :: PaymentPubKeyHash -> Value -> EscrowTarget d
payToPaymentPubKeyTarget = PaymentPubKeyTarget

{- | An 'EscrowTarget' that pays the value to a script address, with the
  given data script.
-}
payToScriptTarget :: ValidatorHash -> Datum -> Value -> EscrowTarget Datum
payToScriptTarget = ScriptTarget

-- | Definition of an escrow contract, consisting of a deadline and a list of targets
data EscrowParams d = EscrowParams
  { escrowDeadline :: POSIXTime
  -- ^ Latest point at which the outputs may be spent.
  , escrowTargets :: [EscrowTarget d]
  -- ^ Where the money should go. For each target, the contract checks that
  --   the output 'mkTxOutput' of the target is present in the spending
  --   transaction.
  }
  deriving (Functor)

PlutusTx.makeLift ''EscrowParams

{- | The total 'Value' that must be paid into the escrow contract
  before it can be unlocked
-}
targetTotal :: EscrowParams d -> Value
targetTotal = foldl (\vl tgt -> vl <> targetValue tgt) mempty . escrowTargets

-- | The 'Value' specified by an 'EscrowTarget'
targetValue :: EscrowTarget d -> Value
targetValue = \case
  PaymentPubKeyTarget _ vl -> vl
  ScriptTarget _ _ vl -> vl

toTxOutValue :: Value -> C.TxOutValue C.ConwayEra
toTxOutValue = either (error . show) C.toCardanoTxOutValue . C.toCardanoValue

toHashableScriptData :: (PlutusTx.ToData a) => a -> C.HashableScriptData
toHashableScriptData = C.unsafeHashableScriptData . C.fromPlutusData . PlutusTx.toData

toTxOutInlineDatum :: (PlutusTx.ToData a) => a -> C.TxOutDatum C.CtxTx C.ConwayEra
toTxOutInlineDatum = C.TxOutDatumInline C.BabbageEraOnwardsConway . toHashableScriptData

toValidityRange
  :: SlotConfig
  -> Interval.Interval POSIXTime
  -> (C.TxValidityLowerBound C.ConwayEra, C.TxValidityUpperBound C.ConwayEra)
toValidityRange slotConfig =
  either (error . show) id . C.toCardanoValidityRange . posixTimeRangeToContainedSlotRange slotConfig

-- | Create a 'Ledger.TxOut' value for the target
mkTxOutput :: EscrowTarget Datum -> C.TxOut C.CtxTx C.ConwayEra
mkTxOutput = \case
  PaymentPubKeyTarget pkh vl ->
    C.TxOut
      ( C.makeShelleyAddressInEra
          C.shelleyBasedEra
          testnet
          (either (error . show) C.PaymentCredentialByKey $ C.toCardanoPaymentKeyHash pkh)
          C.NoStakeAddress
      )
      (toTxOutValue vl)
      C.TxOutDatumNone
      C.ReferenceScriptNone
  ScriptTarget (Ledger.ValidatorHash vs) ds vl ->
    C.TxOut
      ( C.makeShelleyAddressInEra
          C.shelleyBasedEra
          testnet
          (either (error . show) C.PaymentCredentialByScript $ C.toCardanoScriptHash $ Ledger.ScriptHash vs)
          C.NoStakeAddress
      )
      (toTxOutValue vl)
      (toTxOutInlineDatum ds)
      C.ReferenceScriptNone

data Action = Redeem | Refund

data Escrow
instance Scripts.ValidatorTypes Escrow where
  type RedeemerType Escrow = Action
  type DatumType Escrow = PaymentPubKeyHash

PlutusTx.unstableMakeIsData ''Action
PlutusTx.makeLift ''Action

{-# INLINEABLE meetsTarget #-}

{- | @ptx `meetsTarget` tgt@ if @ptx@ pays at least @targetValue tgt@ to the
  target address.

  The reason why this does not require the target amount to be equal
  to the actual amount is to enable any excess funds consumed by the
  spending transaction to be paid to target addresses. This may happen if
  the target address is also used as a change address for the spending
  transaction, and allowing the target to be exceed prevents outsiders from
  poisoning the contract by adding arbitrary outputs to the script address.
-}
meetsTarget :: TxInfo -> EscrowTarget Datum -> Bool
meetsTarget ptx = \case
  PaymentPubKeyTarget pkh vl ->
    valuePaidTo ptx (unPaymentPubKeyHash pkh) `geq` vl
  ScriptTarget validatorHash dataValue vl ->
    case scriptOutputsAt validatorHash ptx of
      [(dataValue', vl')] ->
        PlutusTx.traceIfFalse "dataValue" (dataValue' PlutusTx.== OutputDatum dataValue)
          PlutusTx.&& PlutusTx.traceIfFalse "value" (vl' `geq` vl)
      _ -> False

{-# INLINEABLE validate #-}
validate :: EscrowParams Datum -> PaymentPubKeyHash -> Action -> ScriptContext -> Bool
validate EscrowParams{escrowDeadline, escrowTargets} contributor action ScriptContext{scriptContextTxInfo} =
  case action of
    Redeem ->
      PlutusTx.traceIfFalse
        "escrowDeadline-after"
        (escrowDeadline `Interval.after` txInfoValidRange scriptContextTxInfo)
        PlutusTx.&& PlutusTx.traceIfFalse
          "meetsTarget"
          (PlutusTx.all (meetsTarget scriptContextTxInfo) escrowTargets)
    Refund ->
      PlutusTx.traceIfFalse
        "escrowDeadline-before"
        ((escrowDeadline PlutusTx.- 1000) `Interval.before` txInfoValidRange scriptContextTxInfo)
        PlutusTx.&& PlutusTx.traceIfFalse
          "txSignedBy"
          (scriptContextTxInfo `txSignedBy` unPaymentPubKeyHash contributor)

typedValidator :: EscrowParams Datum -> V2.TypedValidator Escrow
typedValidator =
  go
  where
    go =
      V2.mkTypedValidatorParam @Escrow
        $$(PlutusTx.compile [||validate||])
        $$(PlutusTx.compile [||wrap||])
    wrap = Scripts.mkUntypedValidator


mkEscrowAddress :: EscrowParams Datum -> Ledger.CardanoAddress
mkEscrowAddress = validatorCardanoAddress testnet . typedValidator

-- | Pay some money into the escrow contract.
mkPayTx
  :: SlotConfig
  -> EscrowParams Datum
  -- ^ The escrow contract
  -> Ledger.CardanoAddress
  -- ^ Wallet address
  -> Value
  -- ^ How much money to pay in
  -> (C.CardanoBuildTx, Ledger.UtxoIndex)
mkPayTx slotConfig escrow wallet vl =
  let escrowAddr = mkEscrowAddress escrow
      pkh = Ledger.PaymentPubKeyHash $ fromJust $ Ledger.cardanoPubKeyHash wallet
      txOut = C.TxOut escrowAddr (toTxOutValue vl) (toTxOutInlineDatum pkh) C.ReferenceScriptNone
      validityRange = toValidityRange slotConfig $ Interval.to $ escrowDeadline escrow - 1000
      utx =
        E.emptyTxBodyContent
          { C.txOuts = [txOut]
          , C.txValidityLowerBound = fst validityRange
          , C.txValidityUpperBound = snd validityRange
          }
      utxoIndex = mempty
   in (C.CardanoBuildTx utx, utxoIndex)

pay
  :: (E.MonadEmulator m)
  => Ledger.CardanoAddress
  -> Ledger.PaymentPrivateKey
  -> EscrowParams Datum
  -> Value
  -> m ()
pay wallet privateKey escrow vl = do
  E.logInfo @String $ "Pay " <> show vl <> " to the script"
  slotConfig <- asks pSlotConfig
  let (utx, utxoIndex) = mkPayTx slotConfig escrow wallet vl
  void $ E.submitTxConfirmed utxoIndex wallet [toWitness privateKey] utx

newtype RedeemSuccess = RedeemSuccess TxId
  deriving (Eq, Show)

{- | Redeem all outputs at the contract address using a transaction that
  has all the outputs defined in the contract's list of targets.
-}
mkRedeemTx
  :: (E.MonadEmulator m)
  => EscrowParams Datum
  -> m (C.CardanoBuildTx, Ledger.UtxoIndex)
mkRedeemTx escrow = do
  let escrowAddr = mkEscrowAddress escrow
  unspentOutputs <- E.utxosAt escrowAddr
  slotConfig <- asks pSlotConfig
  current <- fst <$> E.currentTimeRange
  if current >= escrowDeadline escrow
    then throwError $ E.CustomError $ show (RedeemFailed DeadlinePassed)
    else
      if C.fromCardanoValue (foldMap Ledger.cardanoTxOutValue (C.unUTxO unspentOutputs))
        `lt` targetTotal escrow
        then throwError $ E.CustomError $ show (RedeemFailed NotEnoughFundsAtAddress)
        else
          let
            validityRange = toValidityRange slotConfig $ Interval.to $ escrowDeadline escrow - 1000
            txOuts = map mkTxOutput (escrowTargets escrow)
            witnessHeader =
              C.toCardanoTxInScriptWitnessHeader
                (Ledger.getValidator <$> Scripts.vValidatorScript (typedValidator escrow))
            redeemer = toHashableScriptData Redeem
            witness =
              C.BuildTxWith $
                C.ScriptWitness C.ScriptWitnessForSpending $
                  witnessHeader C.InlineScriptDatum redeemer C.zeroExecutionUnits
            txIns = (,witness) <$> Map.keys (C.unUTxO unspentOutputs)
            utx =
              E.emptyTxBodyContent
                { C.txIns = txIns
                , C.txOuts = txOuts
                , C.txValidityLowerBound = fst validityRange
                , C.txValidityUpperBound = snd validityRange
                }
           in
            pure (C.CardanoBuildTx utx, unspentOutputs)

redeem
  :: (E.MonadEmulator m)
  => Ledger.CardanoAddress
  -> Ledger.PaymentPrivateKey
  -> EscrowParams Datum
  -> m RedeemSuccess
redeem wallet privateKey escrow = do
  E.logInfo @String "Redeeming"
  (utx, utxoIndex) <- mkRedeemTx escrow
  RedeemSuccess . getCardanoTxId <$> E.submitTxConfirmed utxoIndex wallet [toWitness privateKey] utx

newtype RefundSuccess = RefundSuccess TxId
  deriving newtype (Eq, Show)

-- | Claim a refund of the contribution.
mkRefundTx
  :: (E.MonadEmulator m)
  => EscrowParams Datum
  -> Ledger.CardanoAddress
  -- ^ Wallet address
  -> m (C.CardanoBuildTx, Ledger.UtxoIndex)
mkRefundTx escrow wallet = do
  let escrowAddr = mkEscrowAddress escrow
  unspentOutputs <- E.utxosAt escrowAddr
  slotConfig <- asks pSlotConfig
  let validityRange = toValidityRange slotConfig $ Interval.from $ escrowDeadline escrow
      pkh = Ledger.PaymentPubKeyHash $ fromJust $ Ledger.cardanoPubKeyHash wallet
      extraKeyWit = either (error . show) id $ C.toCardanoPaymentKeyHash pkh
      pkhDatumHash = datumHash $ Datum $ PlutusTx.toBuiltinData pkh
      ownUnspentOutputs =
        Map.keys $
          Map.filter (\(C.TxOut _aie _tov tod _rs) -> C.fromCardanoTxOutDatumHash' tod == Just pkhDatumHash) $
            C.unUTxO unspentOutputs
      witnessHeader =
        C.toCardanoTxInScriptWitnessHeader
          (Ledger.getValidator <$> Scripts.vValidatorScript (typedValidator escrow))
      redeemer = toHashableScriptData Refund
      witness =
        C.BuildTxWith $
          C.ScriptWitness C.ScriptWitnessForSpending $
            witnessHeader C.InlineScriptDatum redeemer C.zeroExecutionUnits
      txIns = (,witness) <$> ownUnspentOutputs
      utx =
        E.emptyTxBodyContent
          { C.txIns = txIns
          , C.txValidityLowerBound = fst validityRange
          , C.txValidityUpperBound = snd validityRange
          , C.txExtraKeyWits = C.TxExtraKeyWitnesses C.AlonzoEraOnwardsConway [extraKeyWit]
          }
  if null txIns
    then throwError $ E.CustomError $ show RefundFailed
    else pure (C.CardanoBuildTx utx, unspentOutputs)

refund
  :: (E.MonadEmulator m)
  => Ledger.CardanoAddress
  -> Ledger.PaymentPrivateKey
  -> EscrowParams Datum
  -> m RefundSuccess
refund wallet privateKey escrow = do
  E.logInfo @String "Refunding"
  (utx, utxoIndex) <- mkRefundTx escrow wallet
  RefundSuccess . getCardanoTxId <$> E.submitTxConfirmed utxoIndex wallet [toWitness privateKey] utx

-- Submit a transaction attempting to take the refund belonging to the given pk.
mkBadRefundTx
  :: (E.MonadEmulator m)
  => EscrowParams Datum
  -> Ledger.PaymentPubKeyHash
  -> m (C.CardanoBuildTx, Ledger.UtxoIndex)
mkBadRefundTx escrow pkh = do
  let escrowAddr = mkEscrowAddress escrow
  unspentOutputs <- E.utxosAt escrowAddr
  let pkhDatumHash = datumHash $ Datum $ PlutusTx.toBuiltinData pkh
      pkhUnspentOutputs =
        Map.keys $
          Map.filter (\(C.TxOut _aie _tov tod _rs) -> C.fromCardanoTxOutDatumHash' tod == Just pkhDatumHash) $
            C.unUTxO unspentOutputs
      witnessHeader =
        C.toCardanoTxInScriptWitnessHeader
          (Ledger.getValidator <$> Scripts.vValidatorScript (typedValidator escrow))
      redeemer = toHashableScriptData Refund
      witness =
        C.BuildTxWith $
          C.ScriptWitness C.ScriptWitnessForSpending $
            witnessHeader C.InlineScriptDatum redeemer C.zeroExecutionUnits
      txIns = (,witness) <$> pkhUnspentOutputs
      utx =
        E.emptyTxBodyContent
          { C.txIns = txIns
          }
  pure (C.CardanoBuildTx utx, unspentOutputs)

badRefund
  :: (E.MonadEmulator m)
  => Ledger.CardanoAddress
  -> Ledger.PaymentPrivateKey
  -> EscrowParams Datum
  -> Ledger.PaymentPubKeyHash
  -> m ()
badRefund wallet privateKey escrow pkh = do
  E.logInfo @String "Bad refund"
  (utx, utxoIndex) <- mkBadRefundTx escrow pkh
  (void $ E.submitTxConfirmed utxoIndex wallet [toWitness privateKey] utx)
    `catchError` (\err -> E.logError $ "Caught error: " ++ show err)

{- | Pay some money into the escrow contract. Then release all funds to their
  specified targets if enough funds were deposited before the deadline,
  or reclaim the contribution if the goal has not been met.
-}
payRedeemRefund
  :: (E.MonadEmulator m)
  => Ledger.CardanoAddress
  -> Ledger.PaymentPrivateKey
  -> EscrowParams Datum
  -> Value
  -> m (Either RefundSuccess RedeemSuccess)
payRedeemRefund wallet privateKey escrow vl = do
  let escrowAddr = mkEscrowAddress escrow
      go = do
        cur <- E.utxosAt escrowAddr
        let presentVal = C.fromCardanoValue (foldMap Ledger.cardanoTxOutValue (C.unUTxO cur))
        if presentVal `geq` targetTotal escrow
          then Right <$> redeem wallet privateKey escrow
          else do
            time <- snd <$> E.currentTimeRange
            if time >= escrowDeadline escrow
              then Left <$> refund wallet privateKey escrow
              else E.nextSlot >> go
  -- Pay the value 'vl' into the contract
  void $ pay wallet privateKey escrow vl
  go

covIdx :: CoverageIndex
covIdx = getCovIdx $$(PlutusTx.compile [||validate||])
