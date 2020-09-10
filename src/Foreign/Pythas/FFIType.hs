{- |
Module          : Foreign.Pythas.FFIType
Description     : Maps native types to FFI supported ones
Copyright       : (c) Simon Plakolb, 2020
License         : LGPLv3
Maintainer      : s.plakolb@gmail.com
Stability       : beta

    Convert native Haskell types to types supported by the
    Haskell FFI and Pythas. For the wrapping of the respective
    functions see 'Foreign.Pythas.Wrapper'.
 -}
module Foreign.Pythas.FFIType where

import Foreign.Pythas.HTypes (HType(..), stripIO, isIO)
import Foreign.Pythas.Utils (finalizerName, toFFIType', toFFIType, fromFFIType)

typeDef = " :: "
fec = ("foreign export ccall "++)

makeFFIType :: String -> [HType] -> String
makeFFIType funcname ccompattypes = fec funcname ++ typeDef ++ functype
 where functype   = typeConcat $ init ccompattypes
       rettype    = ffiType    $ last ccompattypes
       typeConcat = foldr (\a b -> ffiType a ++ " -> " ++ b) rettype

createFFIType :: [HType] -> [HType]
createFFIType ts =
    let fromT = map fromFFIType $ init ts
        toT   = toFFIType (any isIO fromT) $ last ts
    in  map stripIO fromT ++ [toT]

finalizerExport :: String -> HType -> String
finalizerExport n ht = case ht of
        HIO t -> fec $ finalizerName n
                                  ++ typeDef
                                  ++ ffiType t
                                  ++ " -> IO ()"
        _     -> ""

ffiType :: HType -> String
ffiType ht = case ht of
    HUnit   -> "()"
    HCBool  -> "CBool"
    HChar   -> "Char"
    HCChar  -> "CChar"
    HWChar  -> "HWChar"
    HSChar  -> "CSChar"
    HUChar  -> "CUChar"
    HShort  -> "CShort"
    HUShort -> "CUShort"
    HCInt   -> "CInt"
    HCUInt  -> "CUInt"
    HLong   -> "CLong"
    HULong  -> "CULong"
    HLLong  -> "CLLong"
    HULLong -> "CULLong"
    HCFloat  -> "CFloat"
    HCDouble -> "CDouble"
    HFloat   -> "Float"
    HDouble  -> "Double"
    HInt    -> "CInt"
    HInteger -> "CLLong"
    HCWString -> "CWString"
    HIO ht'  -> "IO " ++ further ht'
    HCArray ht' -> "CArray " ++ further ht'
    HCList ht'  -> "CList " ++ further ht'
    HCTuple hts -> case length hts of
                    2 -> "CTuple2 " ++ furthers hts
                    3 -> "CTuple3 " ++ furthers hts
                    4 -> "CTuple4 " ++ furthers hts
    _ -> fail ("Non C-compatible type \"" ++ show ht ++ "\" in export")
    where further  = (\s -> "(" ++ s ++ ")") . ffiType
          furthers = concatMap ((' ':) . further)

