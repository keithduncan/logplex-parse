module Text.Syslog.Parser (
  parseSyslog,

  LogEntry,
  getPriority,
  getVersion,
  getTimestamp,
  getHostname,
  getAppname,
  getProcessId,
  getMessageId,
  getStructuredData,
  getMessage,

  nonZeroDigit,
) where

import Control.Monad
import Data.Maybe

import Text.ParserCombinators.Parsec

data LogEntry = LogEntry { getPriority :: String
                         , getVersion :: String
                         , getTimestamp :: String
                         , getHostname :: String
                         , getAppname :: String
                         , getProcessId :: String
                         , getMessageId :: String
                         , getStructuredData :: [[String]]
                         , getMessage :: String
                         }

parseSyslog :: String -> Either ParseError LogEntry
parseSyslog = parse syslogLine "(unknown)"

syslogLine = LogEntry <$>
             pri <*>
             version <*>
             (space >> timestamp) <*>
             (space >> hostname) <*>
             (space >> appName) <*>
             (space >> procid) <*>
             (space >> msgid) <*>
             (space >> structuredData) <*>
             (space >> message) <*
             eof

pri = between (char '<') (char '>') (occurrences 1 3 digit)
nonZeroDigit = oneOf "123456789"
version = liftM2 (:) nonZeroDigit (occurrences 0 2 digit)

occurrences min' max' parser
  | min' == max' = count max' parser
occurrences min' max' parser = try (count max' parser) <|> occurrences min' (max'-1) parser

timestamp =
 (nilvalue >> return "")
 <|> (mconcat <$> sequence [fullDate, string "T", fullTime])

nilvalue = string "-"

fullDate = mconcat <$> sequence [count 4 digit, string "-", count 2 digit, string "-", count 2 digit]

fullTime = liftM2 (++) partialTime timeOffset

partialTime = do
 time <- time
 sep2 <- string ":"
 second <- second
 frac <- optionMaybe (liftM2 (++) (string ".") (occurrences 1 6 digit))
 return $ time ++ sep2 ++ second ++ fromMaybe "" frac

timeHour = count 2 digit
timeMinute = count 2 digit
second = count 2 digit
timeOffset = string "Z" <|> timeNumOffset
timeNumOffset = liftM2 (:) (oneOf "+-") time
time = mconcat <$> sequence [timeHour, string ":", timeMinute]

hostname = nilvalue <|> occurrences 1 255 printascii

printascii = oneOf printasciiSet
printasciiSet = toEnum <$> [33..126] :: String

appName = nilvalue <|> occurrences 1 48 printascii
procid = nilvalue <|> occurrences 1 128 printascii
msgid = nilvalue <|> occurrences 1 32 printascii

structuredData = (nilvalue >> return []) <|> many1 sdElement

sdElement = between (char '[') (char ']') (liftM2 (:) sdId (many (space >> sdParam)))

sdId = sdName

sdName = occurrences 1 32 (oneOf (filter (`notElem` "= ]\"") printasciiSet))

sdParam = mconcat <$> sequence [paramName, string "=", string "\"", paramValue, string "\""]

paramName = sdName
paramValue = string ""

message = return "foo bar"
