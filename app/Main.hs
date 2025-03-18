module Main where

import Lib
import System.Environment (getArgs)
import System.IO (readFile)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      input <- readFile fileName
      case parseAndGenerate input of
        Left err -> putStrLn $ "Error: " ++ err
        Right luauCode -> do
          putStrLn "Generated Luau Code:"
          putStrLn luauCode
    _ -> putStrLn "Usage: haskell-to-luau <file.hsu>"