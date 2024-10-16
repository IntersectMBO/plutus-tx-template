{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Spec.Escrow qualified

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "use cases" [
    Spec.Escrow.tests
    ]
