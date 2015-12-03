{-# LANGUAGE FlexibleContexts #-}

module Text.Syslog.Parser (
  parseSyslog,

  LogEntry(..),

  StructuredData(..),
  Key,
  Value,
  Param,

  nonZeroDigit,
) where

import Control.Monad
import Data.Maybe

import Text.ParserCombinators.Parsec

import Data.Time.ISO8601
import Data.Time.Clock
import Text.Printf

data LogEntry = LogEntry { getPriority :: String
                         , getVersion :: String
                         , getTimestamp :: UTCTime
                         , getHostname :: String
                         , getAppname :: String
                         , getProcessId :: String
                         , getMessageId :: String
                         , getStructuredData :: [StructuredData]
                         , getMessage :: String
                         } deriving (Show, Eq)

data StructuredData = StructuredData { getId :: String
                                     , getParams :: [Param]
                                     } deriving (Show, Eq)

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

pri = between (char '<') (char '>') (countBetween 1 3 digit) <?> "priority value"
nonZeroDigit = oneOf "123456789"
version = liftM2 (:) nonZeroDigit (countBetween 0 2 digit) <?> "syslog protocol version"

timestamp = do
  time <- (nilvalue >> return "") <|> (mconcat <$> sequence [fullDate, string "T", fullTime]) <?> "ISO8601 full date time timestamp with timezone"
  case parseISO8601 time of
    Nothing -> fail $ printf "invalid timestamp `%s`" time
    Just t  -> return t

nilvalue = string "-"

fullDate = mconcat <$> sequence [count 4 digit, string "-", count 2 digit, string "-", count 2 digit]

fullTime = liftM2 (++) partialTime timeOffset

partialTime = mconcat <$> sequence [time, string ":", second, fromMaybe "" <$> optionMaybe (liftM2 (++) (string ".") (countBetween 1 6 digit))]

timeHour = count 2 digit
timeMinute = count 2 digit
second = count 2 digit
timeOffset = string "Z" <|> timeNumOffset
timeNumOffset = liftM2 (:) (oneOf "+-") time
time = mconcat <$> sequence [timeHour, string ":", timeMinute]

hostname = nilvalue <|> countBetween 1 255 printascii <?> "hostname"

printascii = oneOf printasciiSet
printasciiSet = toEnum <$> [33..126] :: String

appName = nilvalue <|> countBetween 1 48 printascii <?> "application name"
procid = nilvalue <|> countBetween 1 128 printascii <?> "process id"
msgid = nilvalue <|> countBetween 1 32 printascii <?> "message id"

structuredData = (nilvalue >> return []) <|> many1 sdElement

sdElement = between (char '[') (char ']') (liftM2 StructuredData sdId (many (space >> sdParam)))

sdId = sdName

sdName = countBetween 1 32 (oneOf (filter (`notElem` "= ]\"") printasciiSet))

sdParam = liftM2 (,) paramName (string "=" >> quoted (many1 $ escaped '\\' "\"]"))

quoted = between doubleQuote doubleQuote
doubleQuote = char '"'

paramName = sdName

escaped echar chars = let echars = echar:chars
                       in noneOf echars <|> choice (fmap (try . (char echar >>) . char) echars)

message = msgUtf8 <|> msgAny <?> "message"

msgUtf8 = try (bom >> utf8String)
utf8String = many anyChar
bom = string ['\xEF', '\xBB', '\xBF']

msgAny = utf8String

countBetween min' max' parser
  | min' > max'  = error "min occurences cannot be greater than max occurrences"
  | min' == max' = count max' parser
  | otherwise    = try (count max' parser) <|> countBetween min' (max'-1) parser
