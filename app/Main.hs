{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Arrow ((>>>))
import Data.Foldable
import Data.Version (showVersion)
import Eval (interpret)
import qualified Options.Applicative as O
import Options.Applicative.Extra (helperWith)
import Parser (parseProgram)
import Paths_carbon (version)
import Text.Megaparsec (errorBundlePretty)

data Mode =
  Interpret
  | Version

data Options = Options Mode (Maybe FilePath)

main :: IO ()
main = do
  Options mode maybePath <- O.execParser $ O.info (helper <*> parseOptions) O.fullDesc
  case mode of
    Version -> putStrLn $ showVersion version
    Interpret -> case maybePath of
      Nothing -> putStrLn "error: no file provided to interpret"
      Just path -> (readFile path >>=) $ parseProgram >>> \case
        Left err -> putStrLn $ errorBundlePretty err
        Right ast -> interpret ast
 where
  helper = helperWith $ O.long "help" <> O.short 'h' <> O.help "show this help text"

parseFilePath :: O.Parser FilePath
parseFilePath = O.strArgument $ O.metavar "filepath"

parseOptions :: O.Parser Options
parseOptions = Options <$> parseMode <*> O.optional parseFilePath

parseMode :: O.Parser Mode
parseMode = asum
  [ O.flag' Version $ O.long "version" <> O.short 'v' 
      <> O.help "print the version of carbon installed"
  , pure Interpret 
  ]
