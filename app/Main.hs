module Main where

import qualified Data.Text
import Parser (parseConfig)
import Text.Megaparsec (errorBundlePretty)

-- Sample configuration text
sampleConfig :: String
sampleConfig = unlines
  [ "# This is a comment"
  , "GET https://api.github.com/users/octocat"
  , ""
  , "Header: User-Agent: RestCheck-CLI"
  , "Header: Accept: application/json"
  , ""
  , "EXPECT Status 200"
  , "EXPECT Latency < 500"
  , "EXPECT BodyContains \"login\""
  ]

main :: IO ()
main = do
  let result = parseConfig (Data.Text.pack sampleConfig)
  case result of
    Left err -> putStrLn (errorBundlePretty err)
    Right ast -> do
      putStrLn "--- Parsed Successfully ---"
      print ast
