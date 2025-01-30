{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import AlwaysFail
import Data.ByteString.Short qualified as Short
import Data.Set qualified as Set
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusTx.Blueprint
import System.Environment (getArgs)

myContractBlueprint :: ContractBlueprint
myContractBlueprint =
  MkContractBlueprint
    { contractId = Just "always-fail"
    , contractPreamble = myPreamble
    , contractValidators = Set.singleton myValidator
    , contractDefinitions = NoDefinitions
    }

myPreamble :: Preamble
myPreamble =
  MkPreamble
    { preambleTitle = "Always Fail"
    , preambleDescription =
        Just "Blueprint for a Plutus script that always fails"
    , preambleVersion = "1.0.0"
    , preamblePlutusVersion = PlutusV3
    , preambleLicense = Just "MIT"
    }

myValidator :: ValidatorBlueprint referencedTypes
myValidator =
  MkValidatorBlueprint
    { validatorTitle = "Always Fail"
    , validatorDescription =
        Just "Plutus script that always fails"
    , validatorParameters = []
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Redeemer"
          , argumentDescription = Just "Redeemer for the always fail validator"
          , argumentPurpose = Set.fromList [Spend]
          , argumentSchema = definitionRef @()
          }
    , validatorDatum = Nothing
    , validatorCompiled = do
        let script = alwaysFailScript
        let code = Short.fromShort (serialiseCompiledCode script)
        Just (compiledValidator PlutusV3 code)
    }

writeBlueprintToFile :: FilePath -> IO ()
writeBlueprintToFile path = writeBlueprint path myContractBlueprint

main :: IO ()
main =
  getArgs >>= \case
    [arg] -> writeBlueprintToFile arg
    args -> fail $ "Expects one argument, got " <> show (length args)
