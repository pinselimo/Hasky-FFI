module WrapGoldenTests (
    tests
    ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFile)

import System.FilePath.Glob (glob)
import System.FilePath.Posix (takeBaseName, takeFileName, joinPath)
import Text.Parsec (parse)

import Foreign.Pythas.Parser (parseTypeDef)
import Foreign.Pythas.Wrapper (wrap)
import Foreign.Pythas.Utils (TypeDef(funcN, funcT))

tests = do
    gold <- findSingular
    return $ (testGroup "Singular Golden") $ map testSingular gold

findSingular :: IO [FilePath]
findSingular = glob "test/golden/input/*.golden"

outp :: FilePath -> FilePath
outp fp = joinPath ["test/golden/gold", takeFileName fp]

testSingular :: FilePath -> TestTree
testSingular fp = goldenVsFile
            (takeBaseName fp) -- Test name
            (xmpl fp)      -- Correct output
            (outp fp)      -- File written to by test
            (write_singular_golden fp)
    where xmpl fp = joinPath ["test/golden/output", takeFileName fp]

write_singular_golden :: FilePath -> IO ()
write_singular_golden fp = do
    content <- readFile fp
    let t = parse parseTypeDef (takeBaseName fp) content
    putStrLn $ "testing" ++ show fp
    let output = case t of
                    Right td -> wrap "Test" (funcN td) (funcT td)
                    Left err -> ""
    writeFile (outp fp) output

