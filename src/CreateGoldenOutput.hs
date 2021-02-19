module Main where

import System.FilePath.Glob (glob)
import System.FilePath.Posix (takeBaseName, joinPath)
import Text.Parsec (parse)

import Foreign.Pythas.Parser (parseTypeDef)
import Foreign.Pythas.Utils (TypeDef(funcN, funcT))
import Foreign.Pythas.Wrapper (wrap)

main = do
    allfiles <- glob "../test/golden/input/*"
    largefiles <- glob "../test/golden/input/*.hs"
    let smallfiles = filter (\x -> not $ elem x largefiles) allfiles
    mapM write_golden smallfiles

write_golden fp = do
    content <- readFile fp
    let t = parse parseTypeDef (takeBaseName fp) content
    case t of
        Right td -> do
            let wrapped = wrap "Test" (funcN td) (funcT td)
            let newfp = joinPath ["../test/golden/output", takeBaseName fp]
            writeFile newfp wrapped
        Left err -> putStrLn $ show err

