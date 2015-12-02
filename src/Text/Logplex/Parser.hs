module Text.Logplex.Parser (
  LogEntry,

  priority,
  version,
  timestamp,
  hostname,
  appname,
  processId,
  messageId,
  structuredData,

  parseLogplex,
) where

import Control.Monad

import Text.ParserCombinators.Parsec

data LogEntry = LogEntry { priority :: String
                         , version :: String
                         , timestamp :: String
                         , hostname :: String
                         , appName :: String
                         , processId :: String
                         , messageId :: String
                         , structuredData :: String
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
  space

  return $ LogEntry "10" "1" "2015-11-31T20:00T+11:00" "keiths-macbook-pro.local" "my-app" "420" "" "key=value"

msgLen :: GenParser Char st String
msgLen = liftM2 (:) (oneOf "123456789") (many digit)
