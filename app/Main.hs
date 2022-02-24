module Main where

import Eval (interpret)
import Parser (parseProgram)
import System.Environment
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  [sourcePath] <- getArgs
  input <- readFile sourcePath
  case parseProgram input of
    Left err -> putStrLn $ errorBundlePretty err
    Right ast -> interpret ast
