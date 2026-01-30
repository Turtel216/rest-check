{-# LANGUAGE OverloadedStrings #-}

module Executor where

import Parser  (RequestConfig(..), HttpMethod(..))
import Data.Text(Text, unpack)
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy as LBS
import Data.Time.Clock (getCurrentTime, nominalDiffTimeToSeconds, diffUTCTime)

data ActualResponse = ActualResponse
 { actualStatus :: Int
 , actualBody :: Text
 , actualLatency :: Int
 } deriving (Show)

-- Convert AST Method to ByteString format
methodToBS :: HttpMethod -> BS.ByteString
methodToBS GET = "GET"
methodToBS POST = "POST"
methodToBS PUT = "PUT"
methodToBS DELETE = "DELETE"

-- Convert parser header to RequestHeaders
headersToHeaderList :: [(Text, Text)] -> RequestHeaders
headersToHeaderList = map toHeader
  where
    toHeader (k, v) = (CI.mk (TE.encodeUtf8 k), TE.encodeUtf8 v)

runRequest :: RequestConfig -> IO ActualResponse
runRequest config = do
  -- Parse url
  initialRequest <- parseRequest (unpack $ url config)

  -- Handle body
  let requestBody = case body config of
        Nothing -> initialRequest
        Just b -> setRequestBodyLBS (LBS.fromStrict $ TE.encodeUtf8 b) initialRequest

  -- Set Method and Headers
  let request = setRequestMethod (methodToBS $ method config)
                $ setRequestHeaders (headersToHeaderList $ headers config)
                $ requestBody 

  start <- getCurrentTime 

  response <- httpLBS request

  end <- getCurrentTime

  let latencySecond = nominalDiffTimeToSeconds (diffUTCTime end start)
  let latencyMs = round (latencySecond * 1000)

  let bodyText = TE.decodeUtf8 (LBS.toStrict $ getResponseBody response)

  return ActualResponse
    { actualStatus = statusCode (getResponseStatus response)
    , actualBody = bodyText
    , actualLatency = latencyMs
    }
