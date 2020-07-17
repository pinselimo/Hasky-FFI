module Foreign.Hasky.String (
    CWString, newCWString, peekCWString, freeCWString
) where

import qualified Foreign.C.String as STR
import Foreign.Marshal.Utils (new)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)

type CWString = Ptr STR.CWString

newCWString :: String -> IO CWString
newCWString s = new $ STR.newCWString s

peekCWString :: CWString -> IO String
peekCWString cws = peek cws >>= STR.peekCWString

freeCWString :: CWString -> IO ()
freeCWString cws = peek cws >>= free >> free cws

