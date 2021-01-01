module Foreign.Pythas.Hidden where

import System.FilePath.Posix (dropExtension)
import Text.Parsec.String (parseFromFile)
import Text.Parsec.Error (ParseError)
import Control.Exception (Exception, throw)

import Foreign.Pythas.Parser (parseTypeDefs, parseExports, parseModname)
import Foreign.Pythas.FFICreate (createFFI)
import Foreign.Pythas.Utils (TypeDef(funcN))

newtype PythasException = ParseException ParseError
                         deriving (Show)
instance Exception PythasException

makePythasExportName :: FilePath -> FilePath
makePythasExportName fp = dropExtension fp ++ "_pythas_ffi.hs"

createFileBindings' :: FilePath -> FilePath -> IO FilePath
createFileBindings' fp fp' = let
    check = either (throw . ParseException) return
    in do

    modname  <- check =<< parseFromFile parseModname  fp
    typeDefs <- check =<< parseFromFile parseTypeDefs fp
    expts    <- parseFromFile parseExports fp

    let exports   = either (\_ -> map funcN typeDefs) id expts
        fc = createFFI fp modname exports typeDefs

    writeFile fp' fc
    return fp'

