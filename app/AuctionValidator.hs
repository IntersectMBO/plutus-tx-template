{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module AuctionValidator where

import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1 (Lovelace, POSIXTime, PubKeyHash, Value)
import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import PlutusLedgerApi.V1.Interval (contains)
import PlutusLedgerApi.V1.Value (lovelaceValue)
import PlutusLedgerApi.V2 (
    Datum (..),
    OutputDatum (..),
    ScriptContext (..),
    TxInfo (..),
    TxOut (..),
    from,
    to,
 )
import PlutusLedgerApi.V2.Contexts (getContinuingOutputs)
import PlutusTx
import PlutusTx.Prelude qualified as PlutusTx
import PlutusTx.Show qualified as PlutusTx


data AuctionParams = AuctionParams
    { apSeller :: PubKeyHash
    , apAsset :: Value
    , apMinBid :: Lovelace
    , apEndTime :: POSIXTime
    }
PlutusTx.makeLift ''AuctionParams

data Bid = Bid
    { bBidder :: PubKeyHash
    , bAmount :: Lovelace
    }

PlutusTx.deriveShow ''Bid
PlutusTx.unstableMakeIsData ''Bid

instance PlutusTx.Eq Bid where
    {-# INLINEABLE (==) #-}
    bid == bid' =
        bBidder bid
            PlutusTx.== bBidder bid'
            PlutusTx.&& bAmount bid
            PlutusTx.== bAmount bid'

newtype AuctionDatum = AuctionDatum {adHighestBid :: Maybe Bid}
PlutusTx.unstableMakeIsData ''AuctionDatum

data AuctionRedeemer = NewBid Bid | Payout
PlutusTx.unstableMakeIsData ''AuctionRedeemer

{-# INLINEABLE auctionTypedValidator #-}
auctionTypedValidator ::
    AuctionParams ->
    AuctionDatum ->
    AuctionRedeemer ->
    ScriptContext ->
    Bool
auctionTypedValidator params (AuctionDatum highestBid) redeemer ctx@(ScriptContext txInfo _) =
    PlutusTx.and conditions
  where
    conditions :: [Bool]
    conditions = case redeemer of
        NewBid bid ->
            [ 
              sufficientBid bid
            , 
              validBidTime
            , 
              refundsPreviousHighestBid
            , 
              correctNewDatum bid
            ]
        Payout ->
            [ 
              validPayoutTime
            , 
              sellerGetsHighestBid
            , 
              highestBidderGetsAsset
            ]

    sufficientBid :: Bid -> Bool
    sufficientBid (Bid _ amt) = case highestBid of
        Just (Bid _ amt') -> amt PlutusTx.> amt'
        Nothing -> amt PlutusTx.>= apMinBid params

    validBidTime :: Bool
    validBidTime = to (apEndTime params) `contains` txInfoValidRange txInfo

    refundsPreviousHighestBid :: Bool
    refundsPreviousHighestBid = case highestBid of
        Nothing -> True
        Just (Bid bidder amt) ->
            case PlutusTx.find
                ( \o ->
                    txOutAddress o
                        PlutusTx.== pubKeyHashAddress bidder
                        PlutusTx.&& txOutValue o
                        PlutusTx.== lovelaceValue amt
                )
                (txInfoOutputs txInfo) of
                Just _ -> True
                Nothing -> PlutusTx.traceError ("Not found: refund output")

    correctNewDatum :: Bid -> Bool
    correctNewDatum bid = case getContinuingOutputs ctx of
        [o] -> case txOutDatum o of
            OutputDatum (Datum newDatum) -> case PlutusTx.fromBuiltinData newDatum of
                Just bid' ->
                    PlutusTx.traceIfFalse
                        ( "Invalid output datum: expected "
                            PlutusTx.<> PlutusTx.show bid
                            PlutusTx.<> ", but got "
                            PlutusTx.<> PlutusTx.show bid'
                        )
                        (bid PlutusTx.== bid')
                Nothing ->
                    PlutusTx.traceError
                        ( "Failed to decode output datum: "
                            PlutusTx.<> PlutusTx.show newDatum
                        )
            OutputDatumHash _ ->
                PlutusTx.traceError "Expected OutputDatum, got OutputDatumHash"
            NoOutputDatum ->
                PlutusTx.traceError "Expected OutputDatum, got NoOutputDatum"
        os ->
            PlutusTx.traceError
                ( "Expected exactly one continuing output, got "
                    PlutusTx.<> PlutusTx.show (PlutusTx.length os)
                )

    validPayoutTime :: Bool
    validPayoutTime = from (apEndTime params) `contains` txInfoValidRange txInfo

    sellerGetsHighestBid :: Bool
    sellerGetsHighestBid = case highestBid of
        Nothing -> True
        Just (Bid _ amt) ->
            case PlutusTx.find
                ( \o ->
                    txOutAddress o
                        PlutusTx.== pubKeyHashAddress (apSeller params)
                        PlutusTx.&& txOutValue o
                        PlutusTx.== lovelaceValue amt
                )
                (txInfoOutputs txInfo) of
                Just _ -> True
                Nothing -> PlutusTx.traceError ("Not found: Output paid to seller")

    highestBidderGetsAsset :: Bool
    highestBidderGetsAsset = case highestBid of
        Nothing -> True
        Just (Bid bidder _) ->
            case PlutusTx.find
                ( \o ->
                    txOutAddress o
                        PlutusTx.== pubKeyHashAddress bidder
                        PlutusTx.&& txOutValue o
                        PlutusTx.== apAsset params
                )
                (txInfoOutputs txInfo) of
                Just _ -> True
                Nothing -> PlutusTx.traceError ("Not found: Output paid to highest bidder")

{-# INLINEABLE auctionUntypedValidator #-}
auctionUntypedValidator :: AuctionParams -> BuiltinData -> BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit
auctionUntypedValidator params datum redeemer ctx =
    PlutusTx.check
        ( auctionTypedValidator
            params
            (PlutusTx.unsafeFromBuiltinData datum)
            (PlutusTx.unsafeFromBuiltinData redeemer)
            (PlutusTx.unsafeFromBuiltinData ctx)
        )

auctionValidatorScript ::
    AuctionParams ->
    CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit)
auctionValidatorScript params =
    $$(PlutusTx.compile [||auctionUntypedValidator||])
        `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 params
