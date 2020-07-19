module Properties where

import Test.QuickCheck (Property, Arbitrary, arbitrary)
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import Test.Framework.Providers.API (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Foreign.C.Types (CChar, CInt, CDouble, CLLong)
import Foreign.Storable (Storable)

import Foreign.Hasky.Array

tests = testGroup "Properties" [test_idArray]

test_idArray = testGroup "Identity Array" [
      testProperty "Double"   (prop_idArray :: [Double]  -> Property)
    , testProperty "Int"      (prop_idArray :: [Int]     -> Property)
    , testProperty "Char"     (prop_idArray :: [Char]    -> Property)
    , testProperty "CDouble"  (prop_idArray :: [CDouble] -> Property)
    , testProperty "CInt"     (prop_idArray :: [CInt]    -> Property)
    , testProperty "CChar"    (prop_idArray :: [CChar]   -> Property)
    , testProperty "CLLong"   (prop_idArray :: [CLLong]  -> Property)
    ]

identityArray list = do
    ptr_array <- newArray list
    list'     <- peek ptr_array
    freeArray ptr_array
    return list'

prop_idArray :: (Storable a, Eq a) => [a] -> Property
prop_idArray list = monadicIO $ do
    list' <- run (identityArray list)
    assert (list' == list)

