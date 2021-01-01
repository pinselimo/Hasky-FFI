{- |
Module          : Main
Description     : Creates FFI bindings using Pythas-Types
Copyright       : (c) Simon Plakolb, 2020
License         : LGPLv3
Maintainer      : s.plakolb@gmail.com
Stability       : beta

    This executable allows you to use the Haskell backend of the Python package 'Pythas' independently. It can create FFI bindings from a Haskell source file. The Python and Haskell package are closely related. Some of the provided wrapper types may have unconventional design due to limitations of Python's 'ctypes' library. The type definitions are given in the Pythas-Types package and can be used acress languages.
 -}
module Main where

import System.Environment (getArgs)

import Foreign.Pythas (createFileBindings', makePythasExportName)

{- |
    Parses a Haskell source file at @-i@ and creates a new module for which it will store at @-o@.
    The output file will contain @foreign export ccall@s for all those functions, where wrapping is possible.
    Lists are converted to @Foreign.Pythas.Arrays@, Strings also get their own type. This is due to the handling of these types by Python's 'ctypes'. Tuples with up to four fields can also be wrapped as C structs using the @c-structs@ package.
    It is not necessary to use @Foreign.C.Types@ in the source module. Conversion to these types will be done automatically. It is however necessary to provide type definitions for all functions that should be in the exports. Type synonyms and custom types are not (yet) supported.
 -}
main :: IO ()
main = do
    args <- getArgs
    let (input, output) = parseArgs args


    case input of
      Nothing -> displayHelp
      Just fp ->
          let
          outfp = maybe (makePythasExportName fp) id output
          in createFileBindings' fp outfp >> return ()

parseArgs :: [String] -> (Maybe FilePath, Maybe FilePath)
parseArgs args = (input, output)
    where input = safeHead $ dropWhile (/= "-i") args
          output = safeHead $ dropWhile (/= "-o") args

displayHelp = putStrLn $ "\
    \Pythas-FFI executable\n\
    \---------------------\n\
    \\n\
    \Usage:\n\
    \    pythas-ffi -i Input.hs -o Output.hs\n\
    \or:\n\
    \    pythas-ffi -i Input.hs\n\
    \\n\
    \In the second case the FFI exports will be written to\
    \<Input_pythas_ffi.hs>.\n\
    \\n\
    \Licensed under the LGPLv3 License © 2020, Simon Plakolb"

safeHead (x:_) = Just x
safeHead [] = Nothing

