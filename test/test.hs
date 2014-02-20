module Main where

import           Test.Tasty   (defaultMain)

import qualified Test.Untyped as Untyped

main = defaultMain $ Untyped.tests
