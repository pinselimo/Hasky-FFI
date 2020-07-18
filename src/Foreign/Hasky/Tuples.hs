{- |
Module          : Foreign.Hasky.Tuples
Description     : Tuple types for Python's interfacing library Hasky
Copyright       : (c) Simon Plakolb, 2020
License         : LGPLv3
Maintainer      : s.plakolb@gmail.com
Stability       : beta

    Tuple wrapper types and converting functions. Implementation strongly relies on @Foreign.C.Structs@. 
    The size of tuples that can be created is directly dependent on the size of structs available there.
 -}
module Foreign.Hasky.Tuples (
    CTuple2, newTuple2, peekTuple2,
    CTuple3, newTuple3, peekTuple3,
    CTuple4, newTuple4, peekTuple4
) where

import Foreign.Storable (Storable, peek)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Utils (new)
import Foreign.C.Structs (Struct2(..), Struct3(..), Struct4(..))

-- | Type synonym for a pointer to a struct with two fields.
type CTuple2 a b = Ptr (Struct2 a b)

-- | Creates a @CTuple2@ out of a tuple with two fields. This function allocates space which must be 
-- explicitly freed after usage with @free@ found in @Foreign.Marshal.Alloc@.
newTuple2 :: (Storable a, Storable b) => (a, b) -> IO (CTuple2 a b)
newTuple2 (x, y) = new $ Struct2 x y

-- | (Re-)Creates a common Haskell tuple out of a @CTuple2@. Memory is not freed within this function.
-- If it has been allocated with @newTuple2@ it needs to be free explicitly with @free@.
peekTuple2 :: (Storable a, Storable b) => CTuple2 a b -> IO (a,b)
peekTuple2 ct = do
    s <- peek ct
    return (s2fst s, s2snd s)

-- | Type synonym for a pointer to a struct with three fields.
type CTuple3 a b c = Ptr (Struct3 a b c)

-- | Creates a @CTuple3@ out of a tuple with three fields. This function allocates space which must be 
-- explicitly freed after usage with @free@ found in @Foreign.Marshal.Alloc@.
newTuple3 :: (Storable a, Storable b, Storable c) => (a, b, c) -> IO (CTuple3 a b c)
newTuple3 (x, y, z) = new $ Struct3 x y z

-- | (Re-)Creates a common Haskell tuple out of a @CTuple3@. Memory is not freed within this function.
-- If it has been allocated with @newTuple3@ it needs to be free explicitly with @free@.
peekTuple3 :: (Storable a, Storable b, Storable c) => CTuple3 a b c -> IO (a,b,c)
peekTuple3 ct = do
    s <- peek ct
    return (s3fst s, s3snd s, s3trd s)

-- | Type synonym for a pointer to a struct with four fields.
type CTuple4 a b c d = Ptr (Struct4 a b c d)

-- | Creates a @CTuple4@ out of a tuple with four fields. This function allocates space which must be 
-- explicitly freed after usage with @free@ found in @Foreign.Marshal.Alloc@.
newTuple4 :: (Storable a, Storable b, Storable c, Storable d) => (a, b, c, d) -> IO (CTuple4 a b c d)
newTuple4 (w, x, y, z) = new $ Struct4 w x y z

-- | (Re-)Creates a common Haskell tuple out of a @CTuple4@. Memory is not freed within this function.
-- If it has been allocated with @newTuple4@ it needs to be free explicitly with @free@.
peekTuple4 :: (Storable a, Storable b, Storable c, Storable d) => CTuple4 a b c d -> IO (a,b,c,d)
peekTuple4 ct = do
    s <- peek ct
    return (s4fst s, s4snd s, s4trd s, s4fth s)

