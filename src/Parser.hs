{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Control.Monad(void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


data HttpMethod = GET | POST | PUT | DELETE
  deriving (Show, Eq)

data Expectation
  = ExpectStatus Int
  | ExpectLatency Int      -- Max latency in ms
  | ExpectBodyContains Text
  deriving (Show, Eq)

data RequestConfig = RequestConfig
  { method       :: HttpMethod
  , url          :: Text
  , headers      :: [(Text, Text)]
  , body :: Maybe Text
  , expectations :: [Expectation]
  } deriving (Show, Eq)


-- Parser type (Void means we don't have custom error data)
type Parser = Parsec Void Text

-- Handles whitespace and comments starting with #
sc :: Parser ()
sc = L.space
  space1                         -- Consumes whitespace/newlines
  (L.skipLineComment "#")        -- Consumes line comments
  empty                          -- (No block comments for now)

-- A wrapper to consume trailing whitespace after a parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Matches a specific string literal and consumes trailing space
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- Parses an integer
integer :: Parser Int
integer = lexeme L.decimal

-- Parses a string literal (quoted "like this")
stringLiteral :: Parser Text
stringLiteral = fmap T.pack $ char '"' >> manyTill L.charLiteral (char '"')

-- | Parses: GET, POST, PUT, DELETE
pMethod :: Parser HttpMethod
pMethod = choice
  [ GET    <$ symbol "GET"
  , POST   <$ symbol "POST"
  , PUT    <$ symbol "PUT"
  , DELETE <$ symbol "DELETE"
  ]

pBody :: Parser Text
pBody = do
  _ <- symbol "BODY"

  -- Read until "Expect"
  content <- manyTill anySingle (lookAhead (void (symbol "EXPECT")) <|> eof)
  return $ T.strip $ T.pack content

-- | Parses the URL (non-space characters)
pUrl :: Parser Text
pUrl = lexeme $ do
  urlChars <- takeWhileP (Just "URL character") (\c -> c /= ' ' && c /= '\n' && c /= '\r')
  return urlChars

-- | Parses: Header: Key: Value
-- Example: Header: Authorization: Bearer 123
pHeader :: Parser (Text, Text)
pHeader = do
  _   <- symbol "Header:"
  key <- takeWhileP (Just "Header Key") (\c -> c /= ':')
  _   <- symbol ":"
  val <- lexeme $ takeWhileP (Just "Header Value") (\c -> c /= '\n' && c /= '\r')
  return (T.strip key, T.strip val)

-- | Parses Expectations
-- Examples:
--   EXPECT Status 200
--   EXPECT Latency < 500
--   EXPECT BodyContains "error"
pExpectation :: Parser Expectation
pExpectation = do
  _ <- symbol "EXPECT"
  choice
    [ do
        _ <- symbol "Status"
        code <- integer
        return (ExpectStatus code)
    , do
        _ <- symbol "Latency"
        _ <- optional (symbol "<") -- Optional '<' for readability
        ms <- integer
        return (ExpectLatency ms)
    , do
        _ <- symbol "BodyContains"
        str <- lexeme stringLiteral -- Requires quotes
        return (ExpectBodyContains str)
    ]

-- | The Root Parser
-- Structure:
--   METHOD URL
--   [List of Headers]
--   [List of Expectations]
pConfigFile :: Parser RequestConfig
pConfigFile = do
  sc -- Consume initial whitespace
  m <- pMethod
  u <- pUrl
  -- Parse many headers (try needed because headers/expectations look similar initially)
  hs <- many (try pHeader)
  b <- optional pBody
  ex <- many pExpectation
  eof
  return $ RequestConfig
    { method = m
    , url = u
    , headers = hs
    , body = b
    , expectations = ex
    }

parseConfig :: Text -> Either (ParseErrorBundle Text Void) RequestConfig
parseConfig input = runParser pConfigFile "config.rc" input
