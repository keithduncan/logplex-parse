{-# LANGUAGE FlexibleContexts #-}

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
                         , getStructuredData :: [StructuredData]
                         , getMessage :: String
                         }

data StructuredData = StructuredData { getId :: String
                                     , getParams :: [Param]
                                     }

type Key = String
type Value = String
type Param = (Key, Value)

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
  | min' > max'  = error "you dun goofed"
  | min' == max' = count max' parser
  | otherwise    = try (count max' parser) <|> occurrences min' (max'-1) parser

timestamp = (nilvalue >> return "") <|> (mconcat <$> sequence [fullDate, string "T", fullTime])

nilvalue = string "-"

fullDate = mconcat <$> sequence [count 4 digit, string "-", count 2 digit, string "-", count 2 digit]

fullTime = liftM2 (++) partialTime timeOffset

partialTime = mconcat <$> sequence [time, string ":", second, fromMaybe "" <$> optionMaybe (liftM2 (++) (string ".") (occurrences 1 6 digit))]

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

sdElement = between (char '[') (char ']') (liftM2 StructuredData sdId (many (space >> sdParam)))

sdId = sdName

sdName = occurrences 1 32 (oneOf (filter (`notElem` "= ]\"") printasciiSet))

sdParam = liftM2 (,) paramName (string "=" >> quoted (many1 $ escaped '\\' "\"]"))

quoted = between doubleQuote doubleQuote
doubleQuote = char '"'

paramName = sdName

escaped echar chars = let echars = echar:chars
                       in noneOf echars <|> choice (fmap (try . (char echar >>) . char) echars)

message = return "foo bar"
