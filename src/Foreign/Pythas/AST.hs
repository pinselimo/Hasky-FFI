{- |
Module          : Foreign.Pythas.AST
Description     : Basic ASt to encode wrapping functions
Copyright       : (c) Simon Plakolb, 2020
License         : LGPLv3
Maintainer      : s.plakolb@gmail.com
Stability       : beta

    Encodes a primitive ASt sufficient to describe any
    wrapping and unwrapping of data for FFI interfacing.
 -}
module Foreign.Pythas.AST where

import Foreign.Pythas.HTypes (HType(..), stripIO, isIO)

data AST = Function String [AST] HType
          | Variable String HType
          | Bind     AST AST
          | Bindl    AST AST
          | Lambda   [AST] AST
          | Next     AST AST
          | Tuple    [AST]
          deriving (Eq)

instance Show AST where
    show = showAST

showAST :: AST -> String
showAST h = case h of
    Variable n _ -> ' ':n
    Lambda as bd -> ' ':parens ("\\" ++ concatMap showAST as ++ " -> " ++ showAST bd)
    Bind a b     -> ' ':parens (showAST a ++ " >>=\n    " ++ showAST b)
    Bindl a b    -> showAST a ++ " =<< " ++ showAST b
    Next a b     -> showAST a ++ " >>\n    " ++ showAST b
    Tuple as     -> ' ':parens (foldr ((\a b -> a ++ ", " ++ b) . showAST)
                       (showAST $ last as) $ init as)
    Function n as _ -> ' ':parens (n ++ concatMap showAST as)
    where parens s = '(':s++")"

typeOf :: AST -> HType
typeOf h = let
    io = HIO . stripIO . typeOf
    in case h of
    Function _ _ t -> t
    Variable _ t   -> t
    Bind a b       -> io b
    Bindl a b      -> io a
    Next a b       -> io b
    Tuple as       -> let inner = map typeOf as in
                   if any isIO inner
                   then HIO (HTuple $ map stripIO inner)
                   else HTuple inner
    Lambda as b    -> typeOf b

return' :: AST -> AST
return' hast = case typeOf hast of
    HIO _ -> hast
    _     -> case hast of
        Function _ _ ht  -> ret' ht
        Variable _ ht    -> ret' ht
        Tuple  args      -> ret' $ typeOf hast
        Lambda args body -> Lambda args $ return' body
        _                -> hast
        where ret' = Function "return" [hast] . HIO

add :: AST -> AST -> AST
add hast hast' = case hast of
    Function "return" [f] ft -> Function "return" [add f hast'] ft
    Function fn args ft -> Function fn (hast':args) ft
    Bind a b            -> Bind a $ add b hast'
    Bindl a b           -> Bindl a $ add b hast'
    Next a b            -> Next a $ add b hast'
    Lambda as b         -> Lambda as $ add b hast'

map' :: AST -> AST -> AST
map' f a = case typeOf f of
    (HIO ht)   -> mapM' ht
    HCWString  -> mapM' HCWString
    HCArray a  -> mapM' a
    HCTuple as -> mapM' (HCTuple as)
    ht         -> Function "map"  [mapF f a,a] (HList ht)
    where mapM' ht = Function "mapM" [mapF f a,a] (HIO (HList ht))

mapF :: AST -> AST-> AST
mapF f a = case f of
    Function fn args ft  -> case last args of
            Variable _ _ -> Function fn (init args) ft
            _            -> Lambda [a] f
    _ -> Lambda [a] f

