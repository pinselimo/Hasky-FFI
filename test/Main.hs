module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified ParseUnitTests (tests)
import qualified TypeProperties (tests)
import qualified WrapGoldenTests (tests)

main = do
    wrapperTests <- WrapGoldenTests.tests
    defaultMain $ testGroup "Tasty" [TypeProperties.tests, ParseUnitTests.tests, wrapperTests]

