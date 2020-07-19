{- |
Module          : Foreign.Hasky.Array
Description     : Array type for Python's interfacing library Hasky
Copyright       : (c) Simon Plakolb, 2020
License         : LGPLv3
Maintainer      : s.plakolb@gmail.com
Stability       : beta

    The @Foreign.Hasky.Array@ @CArray@ type is the standard way of wrapping Haskell lists in Hasky.
    Lacking a terminator element for many @Storable@ types, array reading has to be constrained by the array length. To communicate the length of an array across language borders it needs to be packed into a struct containing said length. This enables @peekArray@ to safely read the list contents back from a @CArray@.
    Be aware that while @String@s are @[Char]@s, they are treated seperately in Hasky. See: @Foreign.Hasky.String@.
 -}
module Foreign.Hasky.Array (CArray, newArray, peekArray, freeArray) where

import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.Types (CInt)
import Foreign.C.Structs (Struct2(..))
import Foreign.Storable (Storable, peek)
import Foreign.Marshal.Utils (new)
import Foreign.Marshal.Alloc (free)
import qualified Foreign.Marshal.Array as ARR (newArray, peekArray)


-- | Type synonym for a pointer to a struct with a field of type @CInt@ containing the length of the array and  a pointer to the actual @Foreign.Marshal.Array@. 
type CArray a = Ptr (Struct2 CInt (Ptr a))

-- | Allocates space for a @CArray@ while building it from a Haskell list of @Storable@s. The @CArray@ has to be freed after its use with @freeArray@.
newArray :: (Storable a) => [a] -> IO (CArray a)
newArray xs = ARR.newArray xs >>= new . Struct2 (fromIntegral $ length xs)

-- | (Re-)Creates a Haskell list out of a @CArray@. Memory is not freed within this function. If it had been allocated within Haskell it needs to be freed with @freeArray@.
peekArray :: (Storable a) => CArray a -> IO [a]
peekArray ap = do
    array <- peek ap
    let l = s2fst array
    let a = s2snd array
    if a == nullPtr
      then return []
      else ARR.peekArray (fromIntegral l) a

-- | Frees all memory allocated for a @CArray@ by @newArray@.
freeArray :: (Storable a) => CArray a -> IO ()
freeArray ap = do
    array <- peek ap
    free $ s2snd array
    free ap

