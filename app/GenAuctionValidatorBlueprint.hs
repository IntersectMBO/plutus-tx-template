{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import           AuctionValidator
import qualified Data.ByteString.Short       as Short
import qualified Data.Set                    as Set
import           PlutusLedgerApi.Common      (serialiseCompiledCode)
import qualified PlutusLedgerApi.V1.Crypto   as Crypto
import qualified PlutusLedgerApi.V1.Time     as Time
import qualified PlutusLedgerApi.V1.Value    as Value
import           PlutusTx.Blueprint
import           PlutusTx.Builtins.HasOpaque (stringToBuiltinByteStringHex)
import           System.Environment          (getArgs)

auctionParams :: AuctionParams
auctionParams =
  AuctionParams
    { apSeller =
        -- Replace with the hex-encoded seller's public key hash:
        Crypto.PubKeyHash (
          stringToBuiltinByteStringHex
            "0000000000000000000000000000000000000000\
            \0000000000000000000000000000000000000000"
          )
    , apCurrencySymbol =
        -- Replace with your desired currency symbol (minting policy hash):
        Value.CurrencySymbol (
          stringToBuiltinByteStringHex
            "00000000000000000000000000000000000000000000000000000000"
          )
    , apTokenName =
        -- Replace with your desired token name:
        Value.tokenName "MY_TOKEN"
    , apMinBid =
        -- Minimal bid in lovelace:
        100
    , apEndTime =
        -- Replace with your desired end time in milliseconds:
        Time.fromMilliSeconds 1_725_227_091_000
    }

myContractBlueprint :: ContractBlueprint
myContractBlueprint =
  MkContractBlueprint
    { contractId = Just "auction-validator"
    , contractPreamble = myPreamble
    , contractValidators = Set.singleton myValidator
    , contractDefinitions =
        deriveDefinitions @[AuctionParams, AuctionDatum, AuctionRedeemer]
    }

myPreamble :: Preamble
myPreamble =
  MkPreamble
    { preambleTitle = "Auction Validator"
    , preambleDescription =
        Just "Blueprint for a Plutus script validating auction transactions"
    , preambleVersion = "1.0.0"
    , preamblePlutusVersion = PlutusV2
    , preambleLicense = Just "MIT"
    }

myValidator :: ValidatorBlueprint referencedTypes
myValidator =
  MkValidatorBlueprint
    { validatorTitle = "Auction Validator"
    , validatorDescription =
        Just "Plutus script validating auction transactions"
    , validatorParameters =
        [ MkParameterBlueprint
            { parameterTitle = Just "Parameters"
            , parameterDescription = Just "Compile-time validator parameters"
            , parameterPurpose = Set.singleton Spend
            , parameterSchema = definitionRef @AuctionParams
            }
        ]
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Redeemer"
          , argumentDescription = Just "Redeemer for the auction validator"
          , argumentPurpose = Set.fromList [Spend]
          , argumentSchema = definitionRef @()
          }
    , validatorDatum = Nothing
    , validatorCompiled = do
        let script = auctionValidatorScript auctionParams
        let code = Short.fromShort (serialiseCompiledCode script)
        Just (compiledValidator PlutusV2 code)
    }

writeBlueprintToFile :: FilePath -> IO ()
writeBlueprintToFile path = writeBlueprint path myContractBlueprint

main :: IO ()
main =
  getArgs >>= \case
    [arg] -> writeBlueprintToFile arg
    args -> fail $ "Expects one argument, got " <> show (length args)
