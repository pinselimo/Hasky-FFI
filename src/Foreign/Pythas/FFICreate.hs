module Foreign.Pythas.FFICreate (createFFI) where

import System.FilePath.Posix (dropExtension)

import Foreign.Pythas.Utils (TypeDef(..))
import Foreign.Pythas.FFIType (createFFIType, makeFFIType, finalizerExport)
import Foreign.Pythas.Wrapper (wrap)
import Foreign.Pythas.Finalizer (maybeFinalizerFunc)

imports = map ("import "++)
          ["Foreign.C.Types"
          ,"Foreign.Marshal.Utils (fromBool, toBool)"
          ,"Foreign.Marshal.Alloc (free)"
          ,"Foreign.Storable (peek)"
          ,"Control.Monad (liftM2, liftM3, liftM4)"
          ,"Foreign.C.Structs"
          ,"Foreign.Pythas.Array"
          ,"Foreign.Pythas.List"
          ,"Foreign.Pythas.String"
          ,"Foreign.Pythas.Tuples"
          ]

createFFI :: FilePath -> String -> [String] -> [TypeDef] -> (FilePath, String)
createFFI fn modname exports typeDefs =
 let ffiFilename = dropExtension fn ++ "_hasky_ffi.hs"
     ffiModname = modname ++ "_hasky_ffi"
     exportedFuncTypes = filter ((`elem` exports) . funcN) typeDefs
     ffiFunctions = concatMap (makeFFIExport modname) exportedFuncTypes
     ffiContent = "{-# LANGUAGE ForeignFunctionInterface #-}\n"
             ++ "module " ++ ffiModname
             ++ " where\n\n"
             ++ "import qualified " ++ modname ++ "\n\n"
             ++ foldr (\a b -> a ++ "\n" ++ b) "" (imports ++ [""] ++ ffiFunctions)

 in (ffiFilename, ffiContent)

makeFFIExport :: String -> TypeDef -> [String]
makeFFIExport modname (TypeDef n t) = let
     functype   = createFFIType t
     ffitypedef = makeFFIType n functype
     ffifunc    = wrap modname n t
     maybeFinal = maybeFinalizerFunc n $ last t
     finalizerT = finalizerExport n (last functype)
  in case maybeFinal of
     Just finalizer -> ["",ffitypedef, ffifunc, "", finalizerT, finalizer]
     Nothing        -> ["",ffitypedef, ffifunc]

