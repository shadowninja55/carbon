module Main where

import Eval (interpret)
import Parser (parseProgram)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  input <- readFile "source.cb"
  case parseProgram input of
    Left err -> putStrLn $ errorBundlePretty err
    Right ast -> interpret ast
