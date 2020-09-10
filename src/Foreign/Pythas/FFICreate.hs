{- |
Module          : Foreign.Pythas.FFICreate
Description     : Create FFI interface files for Haskell modules
Copyright       : (c) Simon Plakolb, 2020
License         : LGPLv3
Maintainer      : s.plakolb@gmail.com
Stability       : beta

    Exposes the basic API for the Pythas-FFI functionality.
    Given a Haskell module and some other information can
    create a fitting separate module file exporting FFI
    calls to said prior module.
 -}
module Foreign.Pythas.FFICreate (createFFI) where


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

createFFI :: FilePath -> String -> [String] -> [TypeDef] -> String
createFFI fn modname exports typeDefs =
 let
     ffiModname = modname ++ "_pythas_ffi"
     exportedFuncTypes = filter ((`elem` exports) . funcN) typeDefs
     ffiFunctions = concatMap (makeFFIExport modname) exportedFuncTypes

 in "{-# LANGUAGE ForeignFunctionInterface #-}\n"
 ++ "module " ++ ffiModname
 ++ " where\n\n"
 ++ "import qualified " ++ modname ++ "\n\n"
 ++ foldr (\a b -> a ++ "\n" ++ b) "" (imports ++ [""] ++ ffiFunctions)

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

