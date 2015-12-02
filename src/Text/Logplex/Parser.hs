module Text.Logplex.Parser (
  LogEntry,
  getPriority,
  getVersion,
  getTimestamp,
  getHostname,
  getAppname,
  getProcessId,
  getMessageId,
  getStructuredData,

  parseLogplex,

  parseSyslog,
) where

import Control.Monad

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error

data LogEntry = LogEntry { getPriority :: String
                         , getVersion :: String
                         , getTimestamp :: String
                         , getHostname :: String
                         , getAppname :: String
                         , getProcessId :: String
                         , getMessageId :: String
                         , getStructuredData :: String
                         }

parseLogplex :: String -> Either ParseError [LogEntry]
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
    Left err -> fail $ head $ messageString <$> errorMessages err
    Right le -> return le

msgLen :: GenParser Char st String
msgLen = liftM2 (:) nonZeroDigit (many digit)

--

parseSyslog :: String -> Either ParseError LogEntry
parseSyslog = parse syslogLine "(unknown)"

syslogLine :: GenParser Char st LogEntry
syslogLine = do
  priority <- priVal
  version <- version

  return $ LogEntry "10" "1" "2015-11-31T20:00T+11:00" "keiths-macbook-pro.local" "my-app" "420" "" "key=value"

priVal = between (char '<') (char '>') (occurrences 1 3 digit)
nonZeroDigit = oneOf "123456789"
version = liftM2 (:) nonZeroDigit (occurrences 0 2 digit)

occurrences :: Int -> Int -> GenParser Char st Char -> GenParser Char st String
occurrences min' max' parser
  | min' == max' = count max' parser
occurrences min' max' parser = try (count max' parser) <|> occurrences min' (max'-1) parser
