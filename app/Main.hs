module Main where

import Control.Arrow ((>>>))
import Carbon.Eval
import Carbon.Parser
import Data.Foldable
import Data.Version (showVersion)
import Options.Applicative qualified as O
import Options.Applicative.Extra (helperWith)
import Paths_carbon (version)

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
        Right ast -> interpret ast
        Left err -> putStrLn err
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
