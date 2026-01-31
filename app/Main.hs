{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit( exitFailure, exitSuccess)
import Parser (parseConfig, RequestConfig(..), Expectation(..))
import Executor (runRequest, ActualResponse(..))
import Verifier (verifyAll, VerificationResult(..), CheckStatus(..))
import Text.Megaparsec (errorBundlePretty)
import Control.Monad (forM_, when, unless)
import Control.Concurrent.Async (mapConcurrently)
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Exception (try, SomeException, displayException)

-- | CLI Options
data Options = Options
  { configFiles :: [FilePath] -- ^ Multiple config files
  , verbose :: Bool           -- ^ Verbose output
  , quiet :: Bool             -- ^ Minimal output (only failures)
  , noColor :: Bool           -- ^ Disable colored output 
  , concurrent :: Bool        -- ^ Run files concurrently 
  , timeout :: Maybe Int      -- ^ Request timneout in seconds
  , dryRun :: Bool            -- ^ Parse config wihtout executing
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
  , cBlue :: String
  , cReset :: String
  }

mkColors :: Bool -> Colors
mkColors noCol
  | noCol = Colors "" "" "" "" ""
  | otherwise = Colors "\x1b[32m" "\x1b[31m" "\x1b[33m" "\x1b[34m" "\x1b[0m"


data FileResult = FileResult
  { frFilePath :: FilePath
  , frSuccess :: Bool
  , frMessages :: [ResultMessage]
  } deriving (Show)

data ResultMessage
  = MsgPass String
  | MsgFail String T.Text
  | MsgError String
  | MsgInfo String
  | MsgSkipped String
  deriving (Show)

-- | Process a single config file
processFile :: Options -> FilePath -> IO FileResult
processFile Options{..} filePath = do
  -- Read the file
  fileResult <- try $ TIO.readFile filePath

  case fileResult of
    Left (err :: SomeException) ->
      return $ FileResult filePath False 
        [MsgError $ "Could not read file: " ++ displayException err]

    Right inputContent ->
      case parseConfig inputContent of
        Left err ->
          return $ FileResult filePath False 
            [MsgError $ "Syntax error:\n" ++ errorBundlePretty err]

        Right config
          | dryRun -> do
              -- Dry run: just validate the config
              let infoMsgs = if verbose
                    then [ MsgInfo $ "Method: " ++ show (method config)
                         , MsgInfo $ "URL: " ++ T.unpack (url config)
                         , MsgInfo $ "Expectations: " ++ show (length $ expectations config)
                         ]
                    else []
              return $ FileResult filePath True 
                (infoMsgs ++ [MsgSkipped "Dry run - request not executed"])

          | otherwise -> executeRequest config

  where
    executeRequest config = do
      -- Execute the request
      requestResult <- try $ runRequest config

      case requestResult of
        Left (err :: SomeException) ->
          return $ FileResult filePath False
            [MsgError $ "Request failed: " ++ displayException err]

        Right response -> do
          let results = verifyAll response (expectations config)
          let allPassed = all isPass results
          
          let verboseInfo = if verbose
                then [MsgInfo $ "Response received from " ++ T.unpack (url config)]
                else []

          let messages = verboseInfo ++ map toMessage results
          return $ FileResult filePath allPassed messages

    isPass r = case status r of
      Pass -> True
      _    -> False

    toMessage res = case status res of
      Pass     -> MsgPass (show $ expectation res)
      Fail msg -> MsgFail (show $ expectation res) msg

-- | Print all results after collection
printResults :: Options -> Colors -> [FileResult] -> IO ()
printResults Options{..} colors results = forM_ results $ \FileResult{..} -> do
  let Colors{..} = colors
  
  -- Skip completely successful files in quiet mode
  unless (quiet && frSuccess && not verbose) $ do
    putStrLn $ "\n" ++ cBlue ++ "━━━ " ++ frFilePath ++ " ━━━" ++ cReset
    
    forM_ frMessages $ \msg -> case msg of
      MsgPass desc
        | quiet     -> return ()  -- Skip passes in quiet mode
        | otherwise -> putStrLn $ cGreen ++ "  ✓ " ++ cReset ++ desc
      
      MsgFail desc m -> 
        putStrLn $ cRed ++ "  ✗ " ++ cReset ++ desc ++ "\n      → " ++ T.unpack m
      
      MsgError e -> 
        putStrLn $ cRed ++ "  ⚠ ERROR: " ++ cReset ++ e
      
      MsgInfo info
        | verbose   -> putStrLn $ cYellow ++ "  ℹ " ++ cReset ++ info
        | otherwise -> return ()
      
      MsgSkipped reason ->
        putStrLn $ cYellow ++ "  ⊘ " ++ cReset ++ reason

-- | Print summary statistics
printSummary :: Colors -> [FileResult] -> IO ()
printSummary Colors{..} results = do
  let total  = length results
      passed = length $ filter frSuccess results
      failed = total - passed
  
  putStrLn ""
  putStrLn $ cBlue ++ "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━" ++ cReset
  putStrLn $ "  Total:  " ++ show total ++ " file(s)"
  putStrLn $ cGreen ++ "  Passed: " ++ show passed ++ cReset
  when (failed > 0) $
    putStrLn $ cRed ++ "  Failed: " ++ show failed ++ cReset
  putStrLn $ cBlue ++ "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━" ++ cReset

main :: IO ()
main = do
  opts <- execParser optsInfo
  let colors = mkColors (noColor opts)
      files  = configFiles opts

  -- log following steps in verbose mode
  when (verbose opts) $ do
    putStrLn $ cYellow colors ++ "Processing " ++ show (length files) ++ " file(s)" 
            ++ (if concurrent opts then " concurrently" else " sequentially")
            ++ cReset colors

  -- Process files
  results <- if concurrent opts && length files > 1
    then mapConcurrently (processFile opts) files
    else mapM (processFile opts) files

  -- Print results
  printResults opts colors results
  
  -- Print summary if multiple files or verbose
  when (length files > 1 || verbose opts) $
    printSummary colors results

  -- Exit with appropriate code
  if all frSuccess results
    then exitSuccess
    else exitFailure
