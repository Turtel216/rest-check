module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment(getArgs)
import System.Exit( exitFailure, exitSuccess)
import Parser (parseConfig, RequestConfig(..), Expectation(..))
import Executor (runRequest, ActualResponse(..))
import Verifier (verifyAll, VerificationResult(..), CheckStatus(..))
import Text.Megaparsec (errorBundlePretty)
import Control.Monad (forM_)

-- | Simple ASNI colors
green, red, reset :: String
green = "\x1b[32m"
red = "\x1b[31m"
reset = "\x1b[0m"

main :: IO ()
main = do
  args <- getArgs
  inputContent <- case args of
    [fileName] -> TIO.readFile fileName
    _          -> do
      putStrLn "Usage restcheck <config-file>"
      exitFailure

  putStrLn "--- Reading Config ---"
  case parseConfig inputContent of
    Left err -> do
      putStrLn (red ++ "Syntax Error:" ++ reset)
      putStrLn (errorBundlePretty err)
      exitFailure

    Right config -> do
      -- Execute requests
      putStrLn $ "Running " ++ show (method config) ++ " request to " ++ T.unpack (url config) ++ "..."
      response <- runRequest config

      -- Verify response
      putStrLn "\n--- Verifying Expectation ---"
      let results = verifyAll response (expectations config)

      -- Report results
      let allPassed = all (\r -> case status r of Pass -> True; _ -> False) results

      forM_ results $ \res ->
        case status res of
          Pass -> putStrLn $ green ++ "[PASS] " ++ reset ++ show (expectation res)
          Fail msg -> putStrLn $ red ++ "[FAIL] " ++ reset ++ show (expectation res) ++ " -> " ++ T.unpack msg

      if allPassed
        then exitSuccess
        else exitFailure
