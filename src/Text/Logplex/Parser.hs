module Text.Logplex.Parser (
  parseLogplex,
) where

import Control.Monad
import Data.Maybe

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error

import Text.Syslog.Parser

parseLogplex :: String -> Either ParseError [LogEntry]
parseLogplex = parse logplexDocument "(unknown)"

logplexDocument :: GenParser Char st [LogEntry]
logplexDocument = many frame <* eof

{-
  <https://tools.ietf.org/html/rfc6587#section-3.4.1>
-}

frame :: GenParser Char st LogEntry
frame = do
  len <- read <$> msgLen
  space
  frameContent <- replicateM len anyChar

  case parseSyslog frameContent of
    -- should probably include all the errors?
    Left err -> fail . head $ messageString <$> errorMessages err
    Right le -> return le

msgLen :: GenParser Char st String
msgLen = liftM2 (:) nonZeroDigit (many digit)
