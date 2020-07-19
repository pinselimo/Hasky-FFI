{- |
Module          : Foreign.Hasky.List
Description     : Linked List type for Python's interfacing library Hasky
Copyright       : (c) Simon Plakolb, 2020
License         : LGPLv3
Maintainer      : s.plakolb@gmail.com
Stability       : beta

    Legacy type and functions for linked lists as interface for Haskell lists to Python sequences. The relevant conversion and parsin functions in the Python part of Hasky should still be able to handle a @CList@. It is recommended to wrap lists as @Foreign.Hasky.Array@. Use linked lists only if you have a good reason to do so.
 -}
module Foreign.Hasky.List (CList, newList, peekList, freeList) where

import Foreign.Ptr
import Foreign.Storable (Storable, peek, poke, sizeOf, alignment)
import Foreign.Marshal.Utils (new)
import Foreign.Marshal.Alloc (free)
import Foreign.C.Structs (Struct2(..))

-- | Type synonym for a pointer to a struct with a field of type @a@ and a pointer to another such struct. The final element contains a null pointer in the second field.
type CList a = Ptr (CListElem a)

newtype Storable a => CListElem a = CLE {
    getElem :: Struct2 a (Ptr (CListElem a))
    } deriving (Show, Eq)

instance Storable a => Storable (CListElem a) where
    sizeOf = sizeOf . getElem
    alignment = alignment . getElem
    peek ptr = do
        cstr <- peek $ castPtr ptr
        return $ CLE cstr
    poke ptr cle = do
        poke (castPtr ptr) $ getElem cle

-- | Allocates space for a linked list while building it from a Haskell list of @Storable@s. The @CList@ has to be freed after its use with @freeList@.
newList :: (Storable a) => [a] -> IO (CList a)
newList [] = return nullPtr
newList (x:xs) = newList xs >>= new . CLE . Struct2 x

-- | (Re-)Creates a Haskell list out of a @CList@. Memory is not freed within this function. If it had been allocated within Haskell it needs to be freed with @freeList@.
peekList :: (Storable a) => CList a -> IO [a]
peekList lp = do
    le <- peek lp
    let x = s2fst $ getElem le
    let n = s2snd $ getElem le
    if n == nullPtr
      then return [x]
      else do
        li <- peekList n
        return (x:li)

-- | Frees all memory allocated for a @CList@ by @newList@.
freeList :: (Storable a) => CList a -> IO ()
freeList lp = do
    le <- peek lp
    let n = s2snd $ getElem le
    free lp
    if n == nullPtr
      then return ()
      else freeList n

