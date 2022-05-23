module Main where

import Control.Arrow ((>>>))
import Carbon.Eval
import Carbon.Parser
import Data.Foldable
import Data.Version (showVersion)
import Options.Applicative qualified as O
import Options.Applicative.Extra (helperWith)
import Path
import Paths_carbon (getDataDir, version)

data Mode 
  = Interpret
  | Version

data Options = Options Mode (Maybe FilePath)

main :: IO ()
main = do
  Options mode maybePath <- O.execParser $ O.info (helper <*> parseOptions) O.fullDesc
  case mode of
    Version -> putStrLn $ showVersion version
    Interpret -> case maybePath of
      Nothing -> putStrLn "the carbon repl has not been implemented yet"
      Just path -> do
        source <- readFile path
        dataDir <- getDataDir >>= parseAbsDir
        case parseProgram source of
          Right ast -> interpret (Config dataDir) ast
          Left err -> putStrLn err
 where
  helper = helperWith $ O.long "help" <> O.short 'h' <> O.help "show this help text"

parseOptions :: O.Parser Options
parseOptions = Options <$> parseMode <*> O.optional parseFilePath
 where
  parseMode = asum
    [ O.flag' Version 
      $ O.long "version" 
     <> O.short 'v' 
     <> O.help "print the version of carbon installed"
    , pure Interpret 
    ]
  parseFilePath = O.strArgument $ O.metavar "filepath"
