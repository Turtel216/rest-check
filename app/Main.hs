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

processFile :: Options -> Colors -> FilePath -> IO Bool
processFile opts colors filePath = do
  let Colors{..} = colors

  when (verbose opts) $
    putStrLn $ "--- Processing: " ++ filePath ++ " ---"

  inputContent <- TIO.readFile filePath

  case parseConfig inputContent of
    Left err -> do
      putStrLn $ cRed ++ "Syntax Error in " ++ filePath ++ ":" ++ cReset
      putStrLn (errorBundlePretty err)
      return False

    Right config -> do
      when (dryRun opts) $ do
        putStrLn $ cYellow ++ "[DRY-RUN] " ++ cReset ++ filePath ++ " parsed successfully"
        return ()

      if dryRun opts
        then return True
        else do
          when (verbose opts) $
            putStrLn $ "Running " ++ show (method config) ++ " request to " ++ T.unpack (url config) ++ "..."

          response <- runRequest config
          let results = verifyAll response (expectations config)
          let allPassed = all (\r -> case status r of Pass -> True; _ -> False) results

          unless (quiet opts && allPassed) $
            forM_ results $ \res ->
              case status res of
                Pass -> unless (quiet opts) $
                  putStrLn $ cGreen ++ "[PASS] " ++ cReset ++ show (expectation res)
                Fail msg ->
                  putStrLn $ cRed ++ "[FAIL] " ++ cReset ++ filePath ++ ": " 
                          ++ show (expectation res) ++ " -> " ++ T.unpack msg

          return allPassed
  
main :: IO ()
main = do
  opts <- execParser optsInfo
  let colors = mkColors (noColor opts)
      files = configFiles opts

  results <- if concurrent opts && length files > 1
    then mapConcurrently (processFile opts colors) files
    else mapM (processFile opts colors) files

  let totalFiles = length results
      passedFiles = length (filter id results)

  when (verbose opts || length files > 1) $
    putStrLn $ "\n--- Summary: " ++ show passedFiles ++ "/" ++ show totalFiles ++ " files passed ---"

  if and results
    then exitSuccess
    else exitFailure
