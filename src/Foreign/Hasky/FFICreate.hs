module Foreign.Hasky.FFICreate (createFFI) where

import Foreign.Hasky.ParseTypes (TypeDef(funcN, funcT))
import Foreign.Hasky.FFIType (createFFIType, makeFFIType, finalizerExport)
import Foreign.Hasky.Wrapper (wrap)
import Foreign.Hasky.Finalizer (maybeFinalizerFunc)

imports = map ("import "++)
          ["Foreign.C.Types"
          ,"Foreign.Marshal.Utils (fromBool, toBool)"
          ,"Foreign.Marshal.Alloc (free)"
          ,"Foreign.Storable (peek)"
          ,"Control.Monad (liftM2, liftM3, liftM4)"
          ,"Foreign.C.Structs"
          ,"Foreign.Hasky.Array"
          ,"Foreign.Hasky.List"
          ,"Foreign.Hasky.String"
          ,"Foreign.Hasky.Tuples"
          ]

createFFI :: FilePath -> String -> [String] -> [TypeDef] -> (FilePath, String)
createFFI fn modname exports typeDefs =
 let ffiFilename = takeWhile (/='.') fn ++ "_hasky_ffi.hs"
     ffiModname = modname ++ "_hasky_ffi"
     exportedFuncTypes = filter ((`elem` exports) . funcN) typeDefs
     ffiFunctions = concat $ map (makeFFIExport modname) exportedFuncTypes
     ffiContent = "{-# LANGUAGE ForeignFunctionInterface #-}\n"
             ++ "module " ++ ffiModname
             ++ " where\n\n"
             ++ "import qualified " ++ modname ++ "\n\n"
             ++ foldr (\a b -> a ++ "\n" ++ b) "" (imports ++ [""] ++ ffiFunctions)

 in (ffiFilename, ffiContent)

makeFFIExport :: String -> TypeDef -> [String]
makeFFIExport modname typedef = let
     functype = createFFIType $ funcT typedef
     ffitypedef = makeFFIType (funcN typedef) functype
     ffifunc    = wrap modname (funcN typedef) (funcT typedef)
     maybeFinal = maybeFinalizerFunc (funcN typedef) (last $ funcT typedef)
     finalizerT = finalizerExport (funcN typedef) (last functype)
  in case maybeFinal of
     Just finalizer -> ["",ffitypedef, ffifunc, "", finalizerT, finalizer]
     Nothing        -> ["",ffitypedef, ffifunc]

