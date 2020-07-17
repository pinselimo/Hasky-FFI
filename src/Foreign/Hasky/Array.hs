module Foreign.Hasky.Array (CArray, newArray, peekArray, withArray, freeArray) where

import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.Types (CInt)
import Foreign.C.Structs (Struct2(..))
import Foreign.Storable (Storable, peek)
import Foreign.Marshal.Utils (new)
import Foreign.Marshal.Alloc (free)
import qualified Foreign.Marshal.Array as ARR

type CArray a = Ptr (Struct2 CInt a)

newArray :: (Storable a) => [a] -> IO (CArray a)
newArray xs = ARR.newArray xs >>= new . Struct2 (fromIntegral $ length xs)

peekArray :: (Storable a) => CArray a -> IO [a]
peekArray ap = do
    array <- peek ap
    let l = s2fst array
    let a = s2snd array
    if a == nullPtr
    then return []
    else ARR.peekArray (fromIntegral l) a

withArray :: Storable a => CArray a -> ([a] -> [a]) -> IO (CArray a)
withArray ca f = do
    xs <- peekArray ca
    newArray $ f xs

freeArray :: (Storable a) => CArray a -> IO ()
freeArray ap = do
    array <- peek ap
    free $ s2snd array
    free ap

