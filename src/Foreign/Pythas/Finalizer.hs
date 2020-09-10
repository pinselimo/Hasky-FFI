{- |
Module          : Foreign.Pythas.Finalizer
Description     : Memmory finalizer factory
Copyright       : (c) Simon Plakolb, 2020
License         : LGPLv3
Maintainer      : s.plakolb@gmail.com
Stability       : beta

    Creates functions to release memory allocated
    by the Haskell runtime for wrapped nested types.
 -}
module Foreign.Pythas.Finalizer where

import Control.Applicative(liftA2)
import Data.Maybe (isJust)

import Foreign.Pythas.HTypes (HType(..), stripIO)
import Foreign.Pythas.AST (AST(..), map')
import Foreign.Pythas.Utils (free', fromC, finalizerName, tuple, varA, varB, varC, varD)

maybeFinalizerFunc :: String -> HType -> Maybe String
maybeFinalizerFunc n ht = mkFinalizer <$> maybeFinalizerFunc' (stripIO ht)
    where mkFinalizer h = finalizerName n ++ ' ':varX:" = " ++ show h

maybeFinalizerFunc' :: HType -> Maybe AST
maybeFinalizerFunc' ht = finalize ht (Variable [varX]  ht)

varX = 'x'

finalize :: HType -> AST -> Maybe AST
finalize ht hast = case ht of
    HList a -> freeArray a hast
    HTuple as -> freeTuple as hast
    _       -> free' ht hast

freeArray :: HType -> AST -> Maybe AST
freeArray ht hast = let
    inner  = map' <$> finalize ht hast <*> Just hast
    in case inner of
            Just mp -> Next <$>
                       (Just $ Bind (fromC (HList ht) hast) $ Lambda [hast] mp)
                       <*> free
            Nothing -> free
    where free = free' (HList ht) hast

freeTuple :: [HType] -> AST -> Maybe AST
freeTuple as hast = let
    inner = freeTupleContents as
    in case inner of
        Just inner -> Next <$>
                      (Just $ Bind (fromC (HTuple as) hast) $ Lambda [tuple as] inner)
                      <*> free
        Nothing    -> free
    where free = free' (HTuple as) hast

freeTupleContents :: [HType] -> Maybe AST
freeTupleContents asts = let
        finalizers = filter isJust $ zipWith f asts [varA, varB, varC, varD]
            where f t v = finalize t $ v t
        in case finalizers of
            []   -> Nothing
            x:xs -> foldr (liftA2 Next) x xs

