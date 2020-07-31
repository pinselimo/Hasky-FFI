module ParseUnitTests (
    tests
) where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Text.Parsec (parse, ParseError(..))

import Foreign.Pythas.HTypes
import Foreign.Pythas.Parser (parseIfTypeDef, parseTypeDefs, parseExports, parseModname)
import Foreign.Pythas.Utils (TypeDef(..))

tests = testGroup "UnitTests" [
      parseSimple
    , parseIO
    , parseNested
-- TODO: probably only suited for golden tests   , parseUnsupported
-- TODO:    , testExports
-- TODO:    , testModname
    ]

parseTest = parse parseIfTypeDef "Test"
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
    , "f :: Bool -> [String] -> ([CInt], [Int8])"
    , "f :: ([String], Int) -> [Int]"
    ]

nestedRes = map Right [
      stdTD [HInt, HList HInt, HInt]
    , stdTD [HList (HList HInteger), HTuple [HDouble, HString]]
    , stdTD [HChar, HList (HTuple [HFloat, HCUInt])]
    , stdTD [HBool, HList HString, HTuple [HList HCInt, HList HCChar]]
    , stdTD [HTuple [HList HString, HInt], HList HInt]
    ]

unsupported = [
      "f :: CustomType -> [Int] -> Int"
--    , "f = (1+)"
--    , "f :: Char -> Maybe String"
--    , "f :: Bool -> Either Foo Bar"
    ]

parseSimple = testGroup "Parse Simple Types" $
    zipWith (\str res -> testCase str $ parseTest str @?= res) simple simpleRes

parseIO     = testGroup "Parse IO Types" $
    zipWith (\str res -> testCase str $ parseTest str @?= res) io ioRes

parseNested = testGroup "Parse Nested Types" $
    zipWith (\str res -> testCase str $ parseTest str @?= res) nested nestedRes

parseUnsupported = testGroup "Parse Unsupported Types" $
    map (\str -> testCase str $ (parse parseTypeDefs "" str) @?= Right []) unsupported

