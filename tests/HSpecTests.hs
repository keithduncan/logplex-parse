module Main where

import Text.Logplex.Parser
import Text.Syslog.Parser
import Data.Time.Clock.POSIX
import Test.Hspec

fromRight :: Either a b -> b
fromRight (Left x) = error "not right"
fromRight (Right x) = x

main :: IO ()
main = hspec $ do

  describe "Syslog Parser" $ do
    it "should parse lines" $ do
      let logEntry = fromRight $ parseSyslog "<10>123 2015-12-03T23:12:17+11:00 keiths-macbook-pro.local logplex-parse 420 - [foo bar=\"\\\\\\\"baz\"] troll=lolol"
      logEntry `shouldBe` LogEntry "10" "123" (posixSecondsToUTCTime 1449144737) "keiths-macbook-pro.local" "logplex-parse" "420" "-" [StructuredData "foo" [("bar", "\\\"baz")]] "troll=lolol"

  describe "Logplex Parser" $ do
    it "should parse frames" $ do
      let logEntries = fromRight $ parseLogplex "110 <10>123 2015-12-03T23:12:17+11:00 keiths-macbook-pro.local logplex-parse 420 - [foo bar=\"\\\\\\\"baz\"] troll=lolol"
      logEntries `shouldBe` [LogEntry "10" "123" (posixSecondsToUTCTime 1449144737) "keiths-macbook-pro.local" "logplex-parse" "420" "-" [StructuredData "foo" [("bar", "\\\"baz")]] "troll=lolol"]
