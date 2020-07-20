{-# LANGUAGE ForeignFunctionInterface #-}
module Test_hasky_ffi where

import qualified Test

import Foreign.C.Types
import Foreign.Marshal.Utils (fromBool, toBool)
import Foreign.Marshal.Alloc (free)
import Foreign.Storable (peek)
import Control.Monad (liftM2, liftM3)
import Foreign.C.Structs
import Foreign.HaskyList
import Foreign,HaskyArray
import Foreign.HaskyTuple
import Foreign.HaskyString


foreign export ccall constantInt :: CInt
constantInt =  (fromIntegral (Test.constantInt))

foreign export ccall constantString :: IO (CWString)
constantString =  ( (return (Test.constantString)) >>=
     (\ res ->  (newCWString res)))

foreign export ccall constantStringFinalizer :: CWString -> IO ()
constantStringFinalizer x =  (freeCWString x)

foreign export ccall constantList :: IO (CArray (CInt))
constantList =  ( (return (Test.constantList)) >>=
     (\ res ->  ( (return (map (fromIntegral) res)) >>=
     (\ res ->  (newArray res)))))

foreign export ccall constantListFinalizer :: CArray (CInt) -> IO ()
constantListFinalizer x =  (freeArray x)

foreign export ccall constantTuple :: IO (CTuple2  (CDouble) (CWString))
constantTuple =  ( (return (Test.constantTuple)) >>=
     (\ res ->  ( ( (\ ( a,  b) ->  (liftM2 ((,)) (return (CDouble a)) (newCWString b))) res) >>=
     (\ res ->  (newTuple2 res)))))

foreign export ccall constantTupleFinalizer :: CTuple2  (CDouble) (CWString) -> IO ()
constantTupleFinalizer x =  ( (peekTuple2 x) >>=
     (\ ( a,  b) ->  (freeCWString b))) >>
     (free x)

foreign export ccall constantTriple :: IO (CTuple3  (CLLong) (CWString) (CFloat))
constantTriple =  ( (return (Test.constantTriple)) >>=
     (\ res ->  ( ( (\ ( a,  b,  c) ->  (liftM3 ((,,)) (return (fromIntegral a)) (newCWString b) (return (CFloat c)))) res) >>=
     (\ res ->  (newTuple3 res)))))

foreign export ccall constantTripleFinalizer :: CTuple3  (CLLong) (CWString) (CFloat) -> IO ()
constantTripleFinalizer x =  ( (peekTuple3 x) >>=
     (\ ( a,  b,  c) ->  (freeCWString b))) >>
     (free x)

foreign export ccall sideEffects :: IO (())
sideEffects =  (Test.sideEffects)

foreign export ccall inputSideEffects :: CWString -> IO (())
inputSideEffects a =  ( (peekCWString a) >>=
     (\ a ->  (Test.inputSideEffects a)))

foreign export ccall pureOperationInt :: CInt -> CInt
pureOperationInt a =  (fromIntegral (Test.pureOperationInt (fromIntegral a)))

foreign export ccall pureOperationFloat :: CFloat -> CFloat -> CFloat
pureOperationFloat a b =  (CFloat (Test.pureOperationFloat (realToFrac a) (realToFrac b)))

foreign export ccall pureOperationStrings :: CWString -> IO (CWString)
pureOperationStrings a =  ( ( (peekCWString a) >>=
     (\ a ->  (return (Test.pureOperationStrings a)))) >>=
     (\ res ->  (newCWString res)))

foreign export ccall pureOperationStringsFinalizer :: CWString -> IO ()
pureOperationStringsFinalizer x =  (freeCWString x)

foreign export ccall pureOperationMixed :: CInt -> CDouble -> CDouble
pureOperationMixed a b =  (CDouble (Test.pureOperationMixed (fromIntegral a) (realToFrac b)))

foreign export ccall listOfInteger :: CArray (CLLong) -> IO (CInt)
listOfInteger a =  ( ( ( (peekArray a) >>=
     (\ a ->  (mapM (\ a ->  (return (fromIntegral a))) a))) >>=
     (\ a ->  (return (Test.listOfInteger a)))) >>=
     (\ res ->  (return (fromIntegral res))))

foreign export ccall listMixed :: CArray (CLLong) -> IO (CArray (CDouble))
listMixed a =  ( ( ( (peekArray a) >>=
     (\ a ->  (mapM (\ a ->  (return (fromIntegral a))) a))) >>=
     (\ a ->  (return (Test.listMixed a)))) >>=
     (\ res ->  ( (return (map (CDouble) res)) >>=
     (\ res ->  (newArray res)))))

foreign export ccall listMixedFinalizer :: CArray (CDouble) -> IO ()
listMixedFinalizer x =  (freeArray x)

foreign export ccall listNested :: CArray (CArray (CArray (CWString))) -> IO (CArray (CArray (CArray (CWString))))
listNested a =  ( ( ( (peekArray a) >>=
     (\ a ->  (mapM (\ a ->  ( (peekArray a) >>=
     (\ a ->  (mapM (\ a ->  ( (peekArray a) >>=
     (\ a ->  (mapM (peekCWString) a)))) a)))) a))) >>=
     (\ a ->  (return (Test.listNested a)))) >>=
     (\ res ->  ( (mapM (\ res ->  ( (mapM (\ res ->  ( (mapM (newCWString) res) >>=
     (\ res ->  (newArray res)))) res) >>=
     (\ res ->  (newArray res)))) res) >>=
     (\ res ->  (newArray res)))))

foreign export ccall listNestedFinalizer :: CArray (CArray (CArray (CWString))) -> IO ()
listNestedFinalizer x =  ( (peekArray x) >>=
     (\ x ->  (mapM (\ x ->  ( (peekArray x) >>=
     (\ x ->  (mapM (\ x ->  ( (peekArray x) >>=
     (\ x ->  (mapM (freeCWString) x))) >>
     (freeArray x)) x))) >>
     (freeArray x)) x))) >>
     (freeArray x)

foreign export ccall listOfTuples :: CWString -> CInt -> IO (CArray (CTuple2  (CWString) (CInt)))
listOfTuples a b =  ( ( (peekCWString a) >>=
     (\ a ->  (return (Test.listOfTuples a b)))) >>=
     (\ res ->  ( (mapM (\ res ->  ( ( (\ ( a,  b) ->  (liftM2 ((,)) (newCWString a) (return b))) res) >>=
     (\ res ->  (newTuple2 res)))) res) >>=
     (\ res ->  (newArray res)))))

foreign export ccall listOfTuplesFinalizer :: CArray (CTuple2  (CWString) (CInt)) -> IO ()
listOfTuplesFinalizer x =  ( (peekArray x) >>=
     (\ x ->  (mapM (\ x ->  ( (peekTuple2 x) >>=
     (\ ( a,  b) ->  (freeCWString a))) >>
     (free x)) x))) >>
     (freeArray x)

foreign export ccall listOfTuplesNested :: CWString -> CLLong -> IO (CArray (CArray (CTuple2  (CLLong) (CWString))))
listOfTuplesNested a b =  ( ( (peekCWString a) >>=
     (\ a ->  (return (Test.listOfTuplesNested a (fromIntegral b))))) >>=
     (\ res ->  ( (mapM (\ res ->  ( (mapM (\ res ->  ( ( (\ ( a,  b) ->  (liftM2 ((,)) (return (fromIntegral a)) (newCWString b))) res) >>=
     (\ res ->  (newTuple2 res)))) res) >>=
     (\ res ->  (newArray res)))) res) >>=
     (\ res ->  (newArray res)))))

foreign export ccall listOfTuplesNestedFinalizer :: CArray (CArray (CTuple2  (CLLong) (CWString))) -> IO ()
listOfTuplesNestedFinalizer x =  ( (peekArray x) >>=
     (\ x ->  (mapM (\ x ->  ( (peekArray x) >>=
     (\ x ->  (mapM (\ x ->  ( (peekTuple2 x) >>=
     (\ ( a,  b) ->  (freeCWString b))) >>
     (free x)) x))) >>
     (freeArray x)) x))) >>
     (freeArray x)

foreign export ccall listOfTuplesWithList :: CArray (CWString) -> IO (CArray (CArray (CTuple2  (CArray (CArray (CWString))) (CArray (CLLong)))))
listOfTuplesWithList a =  ( ( ( (peekArray a) >>=
     (\ a ->  (mapM (peekCWString) a))) >>=
     (\ a ->  (return (Test.listOfTuplesWithList a)))) >>=
     (\ res ->  ( (mapM (\ res ->  ( (mapM (\ res ->  ( ( (\ ( a,  b) ->  (liftM2 ((,)) ( (mapM (\ a ->  ( (mapM (newCWString) a) >>=
     (\ a ->  (newArray a)))) a) >>=
     (\ a ->  (newArray a))) ( (return (map (fromIntegral) b)) >>=
     (\ b ->  (newArray b))))) res) >>=
     (\ res ->  (newTuple2 res)))) res) >>=
     (\ res ->  (newArray res)))) res) >>=
     (\ res ->  (newArray res)))))

foreign export ccall listOfTuplesWithListFinalizer :: CArray (CArray (CTuple2  (CArray (CArray (CWString))) (CArray (CLLong)))) -> IO ()
listOfTuplesWithListFinalizer x =  ( (peekArray x) >>=
     (\ x ->  (mapM (\ x ->  ( (peekArray x) >>=
     (\ x ->  (mapM (\ x ->  ( (peekTuple2 x) >>=
     (\ ( a,  b) ->  ( (peekArray a) >>=
     (\ a ->  (mapM (\ a ->  ( (peekArray a) >>=
     (\ a ->  (mapM (freeCWString) a))) >>
     (freeArray a)) a))) >>
     (freeArray a) >>
     (freeArray b))) >>
     (free x)) x))) >>
     (freeArray x)) x))) >>
     (freeArray x)

foreign export ccall tupleWithList :: CInt -> IO (CTuple2  (CArray (CWString)) (CArray (CInt)))
tupleWithList a =  ( (return (Test.tupleWithList (fromIntegral a))) >>=
     (\ res ->  ( ( (\ ( a,  b) ->  (liftM2 ((,)) ( (mapM (newCWString) a) >>=
     (\ a ->  (newArray a))) ( (return (map (fromIntegral) b)) >>=
     (\ b ->  (newArray b))))) res) >>=
     (\ res ->  (newTuple2 res)))))

foreign export ccall tupleWithListFinalizer :: CTuple2  (CArray (CWString)) (CArray (CInt)) -> IO ()
tupleWithListFinalizer x =  ( (peekTuple2 x) >>=
     (\ ( a,  b) ->  ( (peekArray a) >>=
     (\ a ->  (mapM (freeCWString) a))) >>
     (freeArray a) >>
     (freeArray b))) >>
     (free x)

foreign export ccall tupleWithNestedList :: CLLong -> CWString -> IO (CTuple2  (CArray (CArray (CWString))) (CArray (CArray (CLLong))))
tupleWithNestedList a b =  ( ( (peekCWString b) >>=
     (\ b ->  (return (Test.tupleWithNestedList (fromIntegral a) b)))) >>=
     (\ res ->  ( ( (\ ( a,  b) ->  (liftM2 ((,)) ( (mapM (\ a ->  ( (mapM (newCWString) a) >>=
     (\ a ->  (newArray a)))) a) >>=
     (\ a ->  (newArray a))) ( (mapM (\ b ->  ( (return (map (fromIntegral) b)) >>=
     (\ b ->  (newArray b)))) b) >>=
     (\ b ->  (newArray b))))) res) >>=
     (\ res ->  (newTuple2 res)))))

foreign export ccall tupleWithNestedListFinalizer :: CTuple2  (CArray (CArray (CWString))) (CArray (CArray (CLLong))) -> IO ()
tupleWithNestedListFinalizer x =  ( (peekTuple2 x) >>=
     (\ ( a,  b) ->  ( (peekArray a) >>=
     (\ a ->  (mapM (\ a ->  ( (peekArray a) >>=
     (\ a ->  (mapM (freeCWString) a))) >>
     (freeArray a)) a))) >>
     (freeArray a) >>
     ( (peekArray b) >>=
     (\ b ->  (mapM (freeArray) b))) >>
     (freeArray b))) >>
     (free x)

foreign export ccall tupleWithListOfTuples :: CWString -> CWString -> IO (CTuple2  (CArray (CTuple2  (CWString) (CWString))) (CArray (CInt)))
tupleWithListOfTuples a b =  ( ( (peekCWString a) >>=
     (\ a ->  ( (peekCWString b) >>=
     (\ b ->  (return (Test.tupleWithListOfTuples a b)))))) >>=
     (\ res ->  ( ( (\ ( a,  b) ->  (liftM2 ((,)) ( (mapM (\ a ->  ( ( (\ ( a,  b) ->  (liftM2 ((,)) (newCWString a) (newCWString b))) a) >>=
     (\ a ->  (newTuple2 a)))) a) >>=
     (\ a ->  (newArray a))) ( (return (map (fromIntegral) b)) >>=
     (\ b ->  (newArray b))))) res) >>=
     (\ res ->  (newTuple2 res)))))

foreign export ccall tupleWithListOfTuplesFinalizer :: CTuple2  (CArray (CTuple2  (CWString) (CWString))) (CArray (CInt)) -> IO ()
tupleWithListOfTuplesFinalizer x =  ( (peekTuple2 x) >>=
     (\ ( a,  b) ->  ( (peekArray a) >>=
     (\ a ->  (mapM (\ a ->  ( (peekTuple2 a) >>=
     (\ ( a,  b) ->  (freeCWString a) >>
     (freeCWString b))) >>
     (free a)) a))) >>
     (freeArray a) >>
     (freeArray b))) >>
     (free x)
