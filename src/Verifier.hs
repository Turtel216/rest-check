{-# LANGUAGE OverloadedStrings #-}

module Verifier where

import Data.Text (Text)
import qualified Data.Text as T
import Parser (Expectation(..))
import Executor (ActualResponse(..))

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

        -- Check body content
        ExpectBodyContains needle ->
          if needle `T.isInfixOf` actualBody resp
          then Pass
          else Fail $ "Body did not contain string: \"" <> needle <> "\""

  in VerificationResult { expectation = expe, status = resultStatus }

verifyAll :: ActualResponse -> [Expectation] -> [VerificationResult]
verifyAll resp = map (verifySingle resp)
