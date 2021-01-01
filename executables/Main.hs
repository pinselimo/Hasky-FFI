module Main where

import System.Environment (getArgs)

import Foreign.Pythas (createFileBindings', makePythasExportName)

main :: IO ()
main = do
    args <- getArgs
    let (input, output) = parseArgs args


    case input of
      Nothing -> displayHelp
      Just fp ->
          let
          outfp = maybe (makePythasExportName fp) id output
          in createFileBindings' fp outfp >> return ()

parseArgs :: [String] -> (Maybe FilePath, Maybe FilePath)
parseArgs args = (input, output)
    where input = safeHead $ dropWhile (/= "-i") args
          output = safeHead $ dropWhile (/= "-o") args

displayHelp = putStrLn $ "\
    \Pythas-FFI executable\n\
    \---------------------\n\
    \\n\
    \Usage:\n\
    \    pythas-ffi -i Input.hs -o Output.hs\n\
    \or:\n\
    \    pythas-ffi -i Input.hs\n\
    \\n\
    \In the second case the FFI exports will be written to\
    \<Input_pythas_ffi.hs>.\n\
    \\n\
    \Licensed under the LGPLv3 License © 2020, Simon Plakolb"

safeHead (x:_) = Just x
safeHead [] = Nothing

