{-# LANGUAGE ForeignFunctionInterface #-}
module Example_hasky_ffi where

import qualified Example

import Foreign.C.Types
import Foreign.Marshal.Utils (fromBool, toBool)
import Foreign.Marshal.Alloc (free)
import Foreign.Storable (peek)
import Control.Monad (liftM2, liftM3)
import HaskyList
import HaskyArray
import HaskyTuple
import HaskyString


foreign export ccall someConstant :: CInt
someConstant =  (fromIntegral (Example.someConstant))

foreign export ccall hello :: IO (())
hello =  (Example.hello)

foreign export ccall square :: CInt -> CInt
square a =  (Example.square a)

foreign export ccall multisin :: CInt -> CDouble -> CDouble
multisin a b =  (CDouble (Example.multisin (fromIntegral a) (realToFrac b)))

foreign export ccall haskyLen :: CArray (CLLong) -> IO (CInt)
haskyLen a =  ( ( ( (peekArray a) >>=
     (\ a ->  (mapM (\ a ->  (return (fromIntegral a))) a))) >>=
     (\ a ->  (return (Example.haskyLen a)))) >>=
     (\ res ->  (return (fromIntegral res))))

foreign export ccall mapQuarter :: CArray (CLLong) -> IO (CArray (CDouble))
mapQuarter a =  ( ( ( (peekArray a) >>=
     (\ a ->  (mapM (\ a ->  (return (fromIntegral a))) a))) >>=
     (\ a ->  (return (Example.mapQuarter a)))) >>=
     (\ res ->  ( (return (map (CDouble) res)) >>=
     (\ res ->  (newArray res)))))

foreign export ccall mapQuarterFinalizer :: CArray (CDouble) -> IO ()
mapQuarterFinalizer x =  (freeArray x)

foreign export ccall haskellList :: IO (CArray (CInt))
haskellList =  ( (return (Example.haskellList)) >>=
     (\ res ->  ( (return (map (fromIntegral) res)) >>=
     (\ res ->  (newArray res)))))

foreign export ccall haskellListFinalizer :: CArray (CInt) -> IO ()
haskellListFinalizer x =  (freeArray x)

foreign export ccall strings :: CWString -> IO (CWString)
strings a =  ( ( (peekCWString a) >>=
     (\ a ->  (return (Example.strings a)))) >>=
     (\ res ->  (newCWString res)))

foreign export ccall stringsFinalizer :: CWString -> IO ()
stringsFinalizer x =  (freeCWString x)

foreign export ccall nested :: CArray (CArray (CWString)) -> IO (CArray (CArray (CWString)))
nested a =  ( ( ( (peekArray a) >>=
     (\ a ->  (mapM (\ a ->  ( (peekArray a) >>=
     (\ a ->  (mapM (peekCWString) a)))) a))) >>=
     (\ a ->  (return (Example.nested a)))) >>=
     (\ res ->  ( (mapM (\ res ->  ( (mapM (newCWString) res) >>=
     (\ res ->  (newArray res)))) res) >>=
     (\ res ->  (newArray res)))))

foreign export ccall nestedFinalizer :: CArray (CArray (CWString)) -> IO ()
nestedFinalizer x =  ( (peekArray x) >>=
     (\ x ->  (mapM (\ x ->  ( (peekArray x) >>=
     (\ x ->  (mapM (freeCWString) x))) >>
     (freeArray x)) x))) >>
     (freeArray x)

foreign export ccall tuple :: IO (CTuple2  (CInt) (CWString))
tuple =  ( (return (Example.tuple)) >>=
     (\ res ->  ( ( (\ ( a,  b) ->  (liftM2 ((,)) (return a) (newCWString b))) res) >>=
     (\ res ->  (newTuple2 res)))))

foreign export ccall tupleFinalizer :: CTuple2  (CInt) (CWString) -> IO ()
tupleFinalizer x =  ( (peekTuple2 x) >>=
     (\ ( a,  b) ->  (freeCWString b))) >>
     (free x)
