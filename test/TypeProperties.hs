module TypeProperties where

import Test.Tasty.QuickCheck (Property, Arbitrary, arbitrary, testProperty)
import Test.Tasty (testGroup)
import Test.QuickCheck.Monadic (monadicIO, run, assert)

import Foreign.C.Types (CChar, CInt, CDouble, CLLong)
import Foreign.Storable (Storable)

import Foreign.Hasky.Array
import Foreign.Hasky.List

tests = testGroup "Properties" [test_idArray, test_idList]

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
    list'     <- peekArray ptr_array
    freeArray ptr_array
    return list'

prop_idArray :: (Storable a, Eq a) => [a] -> Property
prop_idArray list = monadicIO $ do
    list' <- run (identityArray list)
    assert (list' == list)

test_idList = testGroup "Identity List" [
      testProperty "Double"   (prop_idList :: [Double]  -> Property)
    , testProperty "Int"      (prop_idList :: [Int]     -> Property)
    , testProperty "Char"     (prop_idList :: [Char]    -> Property)
    , testProperty "CDouble"  (prop_idList :: [CDouble] -> Property)
    , testProperty "CInt"     (prop_idList :: [CInt]    -> Property)
    , testProperty "CChar"    (prop_idList :: [CChar]   -> Property)
    , testProperty "CLLong"   (prop_idList :: [CLLong]  -> Property)
    ]

identityList list = do
    ptr_list <- newList list
    list'    <- peekList ptr_list
    freeList ptr_list
    return list'

prop_idList :: (Storable a, Eq a) => [a] -> Property
prop_idList list = monadicIO $ do
    list' <- run (identityList list)
    assert (list' == list)

