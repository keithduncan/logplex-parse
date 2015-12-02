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
  getMessage,

  parseLogplex,

  parseSyslog,
) where

import Control.Monad
import Data.Maybe

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
                         , getMessage :: String
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
  priority <- pri
  version <- version
  space
  timestamp <- timestamp
  space
  hostname <- hostname
  space
  appName <- appName
  space
  procid <- procid
  space
  msgid <- msgid
  space
  structuredData <- structuredData
  return $ LogEntry priority version timestamp hostname appName procid msgid structuredData "foo bar"

pri = between (char '<') (char '>') (occurrences 1 3 digit)
nonZeroDigit = oneOf "123456789"
version = liftM2 (:) nonZeroDigit (occurrences 0 2 digit)

occurrences :: Int -> Int -> GenParser Char st Char -> GenParser Char st String
occurrences min' max' parser
  | min' == max' = count max' parser
occurrences min' max' parser = try (count max' parser) <|> occurrences min' (max'-1) parser

timestamp :: GenParser Char st String
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

structuredData = nilvalue <|> many1 sdElement

sdElement :: GenParser Char st String
sdElement = between (string "[") (string "]") (do
  sdId <- sdId
  elems <- many (do
    space
    sdParam)
  return sdId)

sdId = sdName

sdName :: GenParser Char st String
sdName = occurrences 1 32 (oneOf (filter (`notElem` "= ]\"") printasciiSet))

sdParam = mconcat <$> sequence [paramName, string "=", string "\"", paramValue, string "\""]

paramName = sdName
paramValue = string ""
