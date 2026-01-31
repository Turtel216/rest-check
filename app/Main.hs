{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment(getArgs)
import System.Exit( exitFailure, exitSuccess)
import Parser (parseConfig, RequestConfig(..), Expectation(..))
import Executor (runRequest, ActualResponse(..))
import Verifier (verifyAll, VerificationResult(..), CheckStatus(..))
import Text.Megaparsec (errorBundlePretty)
import Control.Monad (forM_, when, unless)
import Control.Concurrent.Async (mapConcurrently)
import Options.Applicative
import Data.Semigroup ((<>))

-- | CLI Options
data Options = Options
  { configFiles :: [FilePath] -- Multiple config files
  , verbose :: Bool           -- Verbose output
  , quiet :: Bool             -- Minimal output (only failures)
  , noColor :: Bool           -- Run fils concurrently 
  , concurrent :: Bool        -- Request timeout in seconds
  , timeout :: Maybe Int      -- Parse config without executing
  , dryRun :: Bool            -- Parse config wihtout executing
  } deriving (Show)

-- | Parser for CLI options
optionsParser :: Parser Options
optionsParser = Options
  <$> some (argument str
        ( metavar "CONFIG_FILES..."
       <> help "One or more config files to process" ))
  <*> switch
        ( long "verbose"
       <> short 'v'
       <> help "Enable verbose output" )
  <*> switch
        ( long "quiet"
       <> short 'q'
       <> help "Only show failures" )
  <*> switch
        ( long "no-color"
       <> help "Disable colored output" )
  <*> switch
        ( long "concurrent"
       <> short 'c'
       <> help "Process multiple files concurrently" )
  <*> optional (option auto
        ( long "timeout"
       <> short 't'
       <> metavar "SECONDS"
       <> help "Request timeout in seconds" ))
  <*> switch
        ( long "dry-run"
       <> short 'n'
       <> help "Parse config files without executing requests" )

-- | Full parser with help text
optsInfo :: ParserInfo Options
optsInfo = info (optionsParser <**> helper)
  ( fullDesc
 <> progDesc "Run REST API checks from CONFIG_FILES"
 <> header "restcheck - a declarative REST API testing tool" )

-- | Color handling
data Colors = Colors
  { cGreen :: String
  , cRed   :: String
  , cYellow :: String
  , cReset :: String
  }

mkColors :: Bool -> Colors
mkColors noCol
  | noCol = Colors "" "" "" ""
  | otherwise = Colors "\x1b[32m" "\x1b[31m" "\x1b[33m" "\x1b[0m"


data FileResult = FileResult
  { frFilePath :: FilePath
  , frSuccess :: Bool
  , frMessages :: [ResultMessage]
  } deriving (Show)

data ResultMessage
  = MsgPass String
  | MsgFail String T.Text
  | MsgError String
  deriving (Show)

-- | Process file silently, returns structured results
processFileSilent :: Options -> FilePath -> IO FileResult
processFileSilent opts filePath = do
  inputContent <- TIO.readFile filePath

  case parseConfig inputContent of
    Left err -> 
      return $ FileResult filePath False [MsgError (errorBundlePretty err)]

    Right config -> do
      response <- runRequest config
      let results = verifyAll response (expectations config)
      let allPassed = all (\r -> case status r of Pass -> True; _ -> False) results

      let messages = map toMessage results
      return $ FileResult filePath allPassed messages

  where
    toMessage res = case status res of
      Pass     -> MsgPass (show $ expectation res)
      Fail msg -> MsgFail (show $ expectation res) msg

-- | Print all results after collection
printResults :: Colors -> [FileResult] -> IO ()
printResults colors results = forM_ results $ \FileResult{..} -> do
  let Colors{..} = colors
  putStrLn $ "\n--- " ++ frFilePath ++ " ---"
  forM_ frMessages $ \msg -> case msg of
    MsgPass desc    -> putStrLn $ cGreen ++ "[PASS] " ++ cReset ++ desc
    MsgFail desc m  -> putStrLn $ cRed ++ "[FAIL] " ++ cReset ++ desc ++ " -> " ++ T.unpack m
    MsgError e      -> putStrLn $ cRed ++ "[ERROR] " ++ cReset ++ e
  
main :: IO ()
main = do
 opts <- execParser optsInfo
 let colors = mkColors (noColor opts)
     files = configFiles opts

 results <- if concurrent opts && length files > 1
    then mapConcurrently (processFileSilent opts) files
    else mapM (processFileSilent opts) files

 -- Print everything sequentially after all work is done
 printResults colors results

 let allPassed = all frSuccess results
 if allPassed then exitSuccess else exitFailure
