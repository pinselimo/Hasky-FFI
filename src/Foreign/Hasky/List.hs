module Foreign.Hasky.List (CList, newList, peekList, withList, freeList, fromList) where

import Foreign.Ptr
import Foreign.Storable (Storable, peek)
import Foreign.Marshal.Utils (new)
import Foreign.Marshal.Alloc (free)
import Foreign.C.Structs (Struct2(..))

type CList a = Ptr (Struct2 a (CList a))

newList :: (Storable a) => [a] -> IO (CList a)
newList [] = return nullPtr
newList (x:xs) = newList xs >>= new . Struct2 x

peekList :: (Storable a) => CList a -> IO [a]
peekList lp = do
    le <- peek lp
    let x = s2fst le
    let n = s2snd le
    if n == nullPtr
    then return [x]
    else do
        li <- peekList n
        return (x:li)

withList :: Storable a => CList a -> ([a] -> [a]) -> IO (CList a)
withList cl f = do
    xs <- peekList cl
    newList $ f xs

freeList :: (Storable a) => CList a -> IO ()
freeList lp = do
    le <- peek lp
    let n = s2snd le
    free lp
    if n == nullPtr
    then return ()
    else freeList n

