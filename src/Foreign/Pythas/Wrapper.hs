{- |
Module          : Foreign.Pythas.Wrapper
Description     : Wrap Haskell functions in FFI type conversion
Copyright       : (c) Simon Plakolb, 2020
License         : LGPLv3
Maintainer      : s.plakolb@gmail.com
Stability       : beta

    This module contains all the functionality to wrap Haskell functions such
    that their input and output types are converted to interface them via Pythas.
 -}
module Foreign.Pythas.Wrapper where

import Foreign.Pythas.HTypes (HType(..), isIO, stripIO)
import Foreign.Pythas.AST (AST(..), return', map', typeOf, add)
import Foreign.Pythas.Utils (toC, fromC, toFFIType', tuple, varA, varB, varC, varD)

wrap :: String -> String -> [HType] -> String
wrap modname funcname functype = funcname ++ (concat $ map show args) ++ " = " ++ show body
    where body = wrapFunc (modname ++ '.':funcname) functype args
          args = zipWith (\c t -> Variable [c] t) ['a'..'z'] $ init functype

wrapFunc :: String -> [HType] -> [AST] -> AST
wrapFunc fn fts args = wrapAST func ft
    where func = wrapArgs fn fts args
          ft   = last fts

wrapAST :: AST -> HType -> AST
wrapAST func ft
    | bothIO    = if ft == (HIO HUnit)
                then func
                else Bind func (Lambda [res] $ convert res)
    | funcIO    = Bind func (Lambda [res] $ return' $ convert res)
    | outpIO    = Bind (return' func) (Lambda [res] $ convert res)
    | otherwise = convert func
    where res = Variable "res" ft
          convert = convertToC ft
          funcIO  = isIO $ typeOf func
          outpIO  = isIO $ toFFIType' ft
          bothIO  = funcIO && outpIO

wrapArgs :: String -> [HType] -> [AST] -> AST
wrapArgs fn ts args = foldr ($) (mkFunc fn ts) convfuncs
    where convfuncs = zipWith addFromC ts args

mkFunc :: String -> [HType] -> AST
mkFunc fn ts = let
    ioIn  = any isIO $ map toFFIType' $ init ts
    ioOut = isIO $ last ts
    in if ioIn && not ioOut
    then return' norm
    else norm
    where norm = Function fn []  $ last ts

addFromC :: HType -> AST -> AST -> AST
addFromC ht arg f = let
    converter = convertFromC ht arg
                     in if isIO $ typeOf converter
                        then Bind converter (Lambda [arg] $ adf arg)
                        else adf converter
                     where adf = add f

convertFromC :: HType -> AST -> AST
convertFromC ht arg = case ht of
    HList a -> fromArray a arg
    HTuple as -> fromCTuple as arg
    _         -> fromC ht arg

convertToC :: HType -> AST -> AST
convertToC ht arg = case ht of
    HList a   -> toArray a arg
    HTuple as -> toCTuple as arg
    _         -> toC ht arg

fromArray :: HType -> AST -> AST
fromArray ht arg = let
    converter = convertFromC ht arg
    inner     = case converter of
        Variable _ _ -> Nothing
        _            -> Just $ if isIO $ typeOf converter
                               then map' converter arg
                               else map' (return' converter) arg
    in case inner of
        Just inner -> Bind (fromC (HList ht) arg) $ Lambda [arg] inner
        Nothing    -> fromC (HList ht) arg

toArray :: HType -> AST -> AST
toArray ht arg = let
    inner = case (ht, toC ht arg) of
        (HList a, _)        -> Just $ map' (toArray a arg) arg
        (HTuple as, _)      -> Just $ map' (toCTuple as arg) arg
        (_, Function _ _ _) -> Just $ map' (toC ht arg) arg
        _                   -> Nothing
    in case inner of
        Just inner -> Bind (return' inner) (Lambda [arg] toA)
        Nothing    -> toA
    where toA = toC (HList ht) arg

fromCTuple :: [HType] -> AST -> AST
fromCTuple hts arg = let
    cf t v = convertFromC t $ v t
    inner  = case zipWith cf hts [varA, varB, varC, varD] of
        a:b:[]     -> Just $ fromCTuple' [a,b] "(,)" "liftM2"
        a:b:c:[]   -> Just $ fromCTuple' [a,b,c] "(,,)" "liftM3"
        a:b:c:d:[] -> Just $ fromCTuple' [a,b,c,d] "(,,,)" "liftM4"
        _          -> Nothing
    in case inner of
        Just inner -> Bind (fromC (HTuple hts) arg) $ Lambda [tuple hts] inner
        Nothing    -> fromC (HTuple hts) arg

fromCTuple' :: [AST] -> String -> String -> AST
fromCTuple' as f l = let
    ts = map (stripIO . typeOf) as
    t  = HTuple ts
    fromTup  args = Function f args t
    liftM' f as   = Function l (f:as) $ HIO t
    in if ts /= map typeOf as
     then liftM' (fromTup []) $ map return' as
     else return' $ fromTup as

toCTuple :: [HType] -> AST -> AST
toCTuple hts arg = let
    cf t v = convertToC t $ v t
    inner  = case zipWith cf hts [varA, varB, varC, varD] of
        a:b:[]   -> Just $ toCTuple' [a,b] "(,)" "liftM2"
        a:b:c:[] -> Just $ toCTuple' [a,b,c] "(,,)" "liftM3"
        a:b:c:d:[] -> Just $ toCTuple' [a,b,c,d] "(,,,)" "liftM4"
        _        -> Nothing
    in case inner of
        Just inner -> Bind (lambdaf $ return' inner) (Lambda [arg] toT)
        Nothing    -> toT
    where lambdaf body = Function "" [Lambda [tuple hts] body, arg] $ t
          toT = toC t arg
          t = HTuple hts

toCTuple' :: [AST] -> String -> String -> AST
toCTuple' as f l = let
    ts = map (stripIO . typeOf) as
    t  = HTuple ts
    toTup  args = Function f args t
    liftM' f as = Function l (f:as) $ HIO t
    in if ts /= map typeOf as
     then liftM' (toTup []) $ map return' as
     else return' $ toTup as

