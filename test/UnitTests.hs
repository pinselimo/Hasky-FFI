module UnitTests (
    tests
) where

import Test.Framework.Providers.API (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@=?))

import Text.Parsec (parse)

import Foreign.Hasky.HTypes
import Foreign.Hasky.ParseTypes (parseIfTypeDef)
import Foreign.Hasky.ParseExports (parseExports, parseModname)

tests = testGroup "UnitTests" [
      parseSimple
    , parseIO
    , parseNested
    , parseUnsupported
-- TODO:    , testExports
-- TODO:    , testModname
    ]

parseTest = parse parseIfTypeDef "Test" s
stdTD = TypeDef "f"

simple = [
      "f :: Int -> Int -> Int"
    , "f :: Integer -> Double -> String"
    , "f :: Char -> Float"
    , "f :: Bool -> String -> CInt -> Int8"
    ]

simpleRes = map Right [
      stdTD [HInt, HInt, HInt]
    , stdTD [HInteger, HDouble, HString]
    , stdTD [HChar, HFloat]
    , stdTD [HBool, HString, HCInt, HCChar]
    ]

io = [
      "f :: Int -> Int -> IO Int"
    , "f :: Integer -> Double -> IO String"
    , "f :: Char -> IO Float"
    , "f :: Bool -> String -> CInt -> IO Int8"
    ]

ioRes = map Right [
      stdTD [HInt, HInt, HIO HInt]
    , stdTD [HInteger, HDouble, HIO HString]
    , stdTD [HChar, HIO HFloat]
    , stdTD [HBool, HString, HCInt, HIO HCChar]
    ]

nested = [
      "f :: Int -> [Int] -> Int"
    , "f :: [[Integer]] -> (Double, String)"
    , "f :: Char -> [(Float, Word32)]"
    , "f :: Bool -> [String] -> ([CInt]->[Int8])"
    ]

nestedRes = map Right [
      stdTD [HInt, HList HInt, HInt]
    , stdTD [HList (HList HInteger), HTuple [HDouble, HString]]
    , stdTD [HChar, HList (HTuple [HFloat, HUInt])]
    , stdTD [HBool, HList HString, HTuple [HList HCInt, HList HCChar]]
    ]

unsupported = [
      "f :: CustomType -> [Int] -> Int"
    , "f = (1+)"
    , "f :: Char -> Maybe String"
    , "f :: Bool -> Either Foo Bar"
    ]

parseSimple = testGroup "Parse Simple Types" $
    zipWith (\str res -> testCase str $ parseTest str @?= res) simple simpleRes

parseIO     = testGroup "Parse IO Types" $
    zipWith (\str res -> testCase str $ parseTest str @?= res) io ioRes

parseNested = testGroup "Parse Nested Types" $
    zipWith (\str res -> testCase str $ parseTest str @?= res) nested nestedRes

parseUnsupported = testGroup "Parse Unsupported Types" $
    map (\str -> testCase str $ parseTest str @?= Left (ParseError "Test")) unsupported

