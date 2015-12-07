module Text.Logplex.ParserSpec where

import Text.Logplex.Parser
import Text.Syslog.Parser
import Data.Time.Clock.POSIX
import Test.Hspec

fromRight :: Either a b -> b
fromRight (Left _) = error "not right"
fromRight (Right x) = x

spec = do
  describe "Logplex Parser" $ do
    it "should parse frames" $ do
      let logEntries = fromRight $ parseLogplex "110 <10>123 2015-12-03T23:12:17+11:00 keiths-macbook-pro.local logplex-parse 420 - [foo bar=\"\\\\\\\"baz\"] troll=lolol"
      logEntries `shouldBe` [LogEntry "10" "123" (posixSecondsToUTCTime 1449144737) "keiths-macbook-pro.local" "logplex-parse" "420" "-" [StructuredData "foo" [("bar", "\\\"baz")]] (Just "troll=lolol")]
