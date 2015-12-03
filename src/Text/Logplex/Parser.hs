module Text.Logplex.Parser (
  parseLogplex,
) where

import Control.Monad
import Data.Maybe

import Data.Text as T
import Data.List as L

import Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec.Error

import Text.Syslog.Parser

parseLogplex :: Text -> Either ParseError [LogEntry]
parseLogplex = parse logplexDocument "(unknown)"

logplexDocument :: GenParser Char st [LogEntry]
logplexDocument = do
  result <- many frame
  eof
  return result

{-
  <https://tools.ietf.org/html/rfc6587#section-3.4.1>
-}

frame :: GenParser Char st LogEntry
frame = do
  len <- msgLen
  let leni = read len :: Int

  space
  frameContent <- replicateM leni anyChar

  case parseSyslog frameContent of
    -- should probably include all the errors?
    Left err -> fail $ L.head $ messageString <$> errorMessages err
    Right le -> return le

msgLen :: GenParser Char st String
msgLen = liftM2 (:) nonZeroDigit (many digit)
