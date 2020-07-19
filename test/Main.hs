module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified ParseUnitTests (tests)
import qualified TypeProperties (tests)

main = defaultMain $ testGroup "Tasty" [TypeProperties.tests, ParseUnitTests.tests]

