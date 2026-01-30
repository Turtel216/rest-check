{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Verifier where

import Data.Text (Text)
import qualified Data.Text as T
import Parser (Expectation(..))
import Executor (ActualResponse(..))
import Control.Lens
import Data.Aeson (Value(..), decode)
import Data.Aeson.Lens(key)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson.Key as Key

data CheckStatus = Pass | Fail Text
  deriving (Show, Eq)

data VerificationResult = VerificationResult
  { expectation ::Expectation 
  , status :: CheckStatus
  } deriving (Show)

-- | verifySingle checks one rule against the response
verifySingle :: ActualResponse -> Expectation -> VerificationResult
verifySingle resp expe =
  let resultStatus = case expe of
        -- Check Status code
        ExpectStatus expectedCode ->
          if actualStatus resp == expectedCode
          then Pass
          else Fail $ T.pack $
               "Expected status " ++ show expectedCode ++
               " but got " ++ show (actualStatus resp)

        -- Check Latency
        ExpectLatency maxMs ->
          if actualLatency resp < maxMs
          then Pass
          else Fail $ T.pack $
               "Expected Latency < " ++ show maxMs ++ "ms" ++
               " but took " ++ show (actualLatency resp) ++ "ms"

        ExpectJsonPath path expectedVal ->
          case decode (LBS.fromStrict $ TE.encodeUtf8 $ actualBody resp) of
            Nothing -> Fail "Response body is not valid JSON"
            Just jsonBody ->
              let extracted = preview(pathToLens path) jsonBody
                  in case extracted of
                       Nothing -> Fail $ "Path '" <> path <> "' not found in JSON"
                       Just actualVal ->
                         if actualVal == expectedVal
                         then Pass
                         else Fail $ T.pack $
                              "Path " ++ show path ++ show expectedVal ++
                              "\n\tExpected: " ++ show expectedVal ++
                              "\n\tGot       " ++ show actualVal
            
        -- Check body content
        ExpectBodyContains needle ->
         if needle `T.isInfixOf` actualBody resp
         then Pass
         else Fail $ "Body did not contain string: \"" <> needle <> "\""

  in VerificationResult { expectation = expe, status = resultStatus }

verifyAll :: ActualResponse -> [Expectation] -> [VerificationResult]
verifyAll resp = map (verifySingle resp)


pathToLens :: Text -> Traversal' Value Value
pathToLens path = foldl1 (.) $ map (key . Key.fromText) (T.splitOn "." path)
