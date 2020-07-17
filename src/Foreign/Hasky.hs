module Foreign.Hasky where

import Text.Parsec.String (parseFromFile)
import Text.Parsec.Error (ParseError)
import Control.Exception (Exception, throw)

import Foreign.Hasky.ParseTypes (parseTypeDefs, TypeDef(funcN))
import Foreign.Hasky.ParseExports (parseExports, parseModname)
import Foreign.Hasky.FFICreate (createFFI)

createFileBindings :: FilePath -> IO FilePath
createFileBindings fp = do
    modn <- parseFromFile parseModname fp
    modname <- case modn of
                Left e -> throw $ ParseException e
                Right modname -> return modname
    tpds <- parseFromFile parseTypeDefs fp
    typeDefs <- case tpds of
                Left e -> throw $ ParseException e
                Right ts -> return ts
    expts <- parseFromFile parseExports fp
    let exports = case expts of
                     Left e  -> map funcN typeDefs
                     Right e -> e
    let (fp', fc) = createFFI fp modname exports typeDefs
    writeFile fp' fc
    return fp'

