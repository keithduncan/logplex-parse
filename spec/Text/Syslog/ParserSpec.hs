module Text.Syslog.ParserSpec where

import Text.Syslog.Parser
import Data.Time.Clock.POSIX
import Test.Hspec

spec = do
  describe "Syslog Parser" $ do
    it "should parse lines" $ do
      let logEntry = parseSyslog "<10>123 2015-12-03T23:12:17+11:00 keiths-macbook-pro.local logplex-parse 420 - [foo bar=\"\\\\\\\"baz\"] troll=lolol"
      logEntry `shouldBe` Right (LogEntry "10" "123" (posixSecondsToUTCTime 1449144737) "keiths-macbook-pro.local" "logplex-parse" "420" "-" [StructuredData "foo" [("bar", "\\\"baz")]] (Just "troll=lolol"))
