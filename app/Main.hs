module Main where

import qualified Data.Text as T
import Parser (parseConfig)
import Executor (runRequest)
import Text.Megaparsec (errorBundlePretty)

-- Sample configuration text
sampleConfig :: String
sampleConfig = unlines
  [ "# This is a comment"
  , "GET https://httpbin.org/get"
  , ""
  , "Header: User-Agent: RestCheck-CLI"
  , ""
  , "EXPECT Status 200"
  , "EXPECT Latency < 500"
  ]

main :: IO ()
main = do
  putStrLn "--- 1. Parsing Config ---"
  let result = parseConfig (T.pack sampleConfig)
  
  case result of
    Left err -> putStrLn (errorBundlePretty err)
    Right ast -> do
      putStrLn "Parsed successfully. AST:"
      print ast
      
      putStrLn "\n--- 2. Executing Request ---"
      actual <- runRequest ast
      
      putStrLn "Response received:"
      print actual
