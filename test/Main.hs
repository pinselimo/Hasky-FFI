module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified ParseUnitTests (tests)
import qualified WrapGoldenTests (tests)

main = do
    wrapperTests <- WrapGoldenTests.tests
    defaultMain $ testGroup "Tasty" [ParseUnitTests.tests, wrapperTests]

