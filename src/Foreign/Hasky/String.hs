{- |
Module          : Foreign.Hasky.String
Description     : String type for Python's interfacing library Hasky
Copyright       : (c) Simon Plakolb, 2020
License         : LGPLv3
Maintainer      : s.plakolb@gmail.com
Stability       : beta

    Wrapper module for @String@s in Hasky. This module mainly exists because Python's 'ctpyes' library implicitly converts a @Ptr CWChar@ to a Python string. In effect, strings handed over from Haskell cannot be freed by Hasky.
    The simple solution to this problem is, to hand over a @Ptr (Ptr CWChar)@ and declare a type synonym for it.
    @CWChar@s are used because its the standard Python string encoding. Using lists (see @Foreign.Hasky.Array@) of @CChar@ provides equivalents for Python's bytestrings.

    For documentation on @CWString@s also refer to @Foreign.C.String@.
 -}
module Foreign.Hasky.String (
    CWString, newCWString, peekCWString, freeCWString
) where

import qualified Foreign.C.String as STR
import Foreign.Marshal.Utils (new)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)

-- | A @Foreign.Hasky.CWString@ is a pointer to a @Foreign.C.String.CWString@. It needs to be freed after use by @freeCWString@.
type CWString = Ptr STR.CWString

-- | Creates the usual NIL terminated @CWString@ from a Haskell @[Char]@ and wraps it in a @Ptr@. This allocates memory twice. The new @CWString@ has to be explicitly freed with @freeCWString@, @free@ will not free the entiry allocated space!
newCWString :: String -> IO CWString
newCWString s = STR.newCWString s >>= new

-- | (Re-)Creates a Haskell @[Char]@ from a @CWString@. Memory is not free within this function.
peekCWString :: CWString -> IO String
peekCWString cws = peek cws >>= STR.peekCWString

-- | Free a @CWString@ by first freeing the actual @Foreign.C.String@ and then the pointer wrapping it.
freeCWString :: CWString -> IO ()
freeCWString cws = peek cws >>= free >> free cws

