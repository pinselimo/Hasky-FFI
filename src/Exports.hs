{-# LANGUAGE ForeignFunctionInterface #-}
module Exports where

import Foreign.Marshal.Alloc (free)
import Foreign.C.String (CWString, peekCWString, newCWString)

import qualified Foreign.Hasky (createFileBindings)

foreign export ccall createFileBindings :: CWString -> IO CWString
foreign export ccall freeReturnedString :: CWString -> IO ()

createFileBindings :: CWString -> IO CWString
createFileBindings cfn = do
    fn <- peekCWString cfn
    fn' <- Foreign.Hasky.createFileBindings fn
    newCWString fn'

freeReturnedString :: CWString -> IO ()
freeReturnedString = free

