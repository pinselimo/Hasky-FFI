{-# OPTIONS_HADDOCK prune #-}
{- |
Module          : Foreign.Pythas
Description     : Haskell backend for Python's Pythas
Copyright       : (c) Simon Plakolb, 2020
License         : LGPLv3
Maintainer      : s.plakolb@gmail.com
Stability       : beta

    This package contains the Haskell backend to the Python package 'Pythas'. It contains wrapper functions and can create FFI bindings from a Haskell source file. The Python and Haskell package are closely related. Some of the provided wrapper types may have unconventional design due to limitations of Python's 'ctypes' library.
    This package is mainly provided to give developers the possibility to exporiment with Pythas's types within a Haskell environment. The Python library contains the source files as well and will compile them itself in a local directory.
 -}
module Foreign.Pythas (
                        createFileBindings
                      , createFileBindings'
                      , makePythasExportName
                      ) where

import Foreign.Pythas.Hidden (createFileBindings', makePythasExportName)

{- |
    Parses a Haskell source file at @fp@ and creates a new module for which it will return the @FilePath@.
    The new file will be located in the same diretory and contain @foreign export ccall@s for all those functions, where wrapping is possible.
    Lists are converted to @Foreign.Pythas.Arrays@, Strings also get their own type. This is due to the handling of these types by Python's 'ctypes'. Tuples with up to four fields can also be wrapped as C structs using the @c-structs@ package.
    It is not necessary to use @Foreign.C.Types@ in the source module. Conversion to these types will be done automatically. It is however necessary to provide type definitions for all functions that should be in the exports. Type synonyms and custom types are not (yet) supported.
 -}
createFileBindings :: FilePath -> IO FilePath
createFileBindings fp = createFileBindings' fp $ makePythasExportName fp

