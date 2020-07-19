module WrapGoldenTests (
    tests
    ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFile)

import System.FilePath.Glob (glob)
import System.FilePath.Posix (takeBaseName, joinPath)
import Text.Parsec (parse)

import Foreign.Hasky.ParseTypes (parseTypeDef, TypeDef(funcN, funcT))
import Foreign.Hasky.Wrapper (wrap)

tests = do
    gold <- findSingular
    return $ (testGroup "Singular Golden") $ map testSingular gold

findSingular :: IO [FilePath]
findSingular = do
    allfiles <- glob "test/golden/input/*"
    largefiles <- glob "test/golden/input/*.hs"
    return $ filter (\x -> not $ elem x largefiles) allfiles

outp :: FilePath -> FilePath
outp fp = joinPath ["test/golden/gold", takeBaseName fp]

testSingular :: FilePath -> TestTree
testSingular fp = goldenVsFile
            (takeBaseName fp) -- Test name
            (xmpl fp)      -- Correct output
            (outp fp)      -- File written to by test
            (write_singular_golden fp)
    where xmpl fp = joinPath ["test/golden/output", takeBaseName fp]

write_singular_golden :: FilePath -> IO ()
write_singular_golden fp = do
    content <- readFile fp
    let t = parse parseTypeDef (takeBaseName fp) content
    putStrLn $ "testing" ++ show fp
    let output = case t of
                    Right td -> wrap "Test" (funcN td) (funcT td)
                    Left err -> ""
    writeFile (outp fp) output

