module Text.Logplex.ParserSpec where

import Text.Logplex.Parser
import Text.Syslog.Parser
import Data.Time.Clock.POSIX
import Test.Hspec

spec = do
  describe "Logplex Parser" $ do
    it "should parse frames" $ do
      let logEntries = parseLogplex "110 <10>123 2015-12-03T23:12:17+11:00 keiths-macbook-pro.local logplex-parse 420 - [foo bar=\"\\\\\\\"baz\"] troll=lolol110 <10>123 2015-12-03T23:12:17+11:00 keiths-macbook-pro.local logplex-parse 420 - [foo bar=\"\\\\\\\"baz\"] troll=lolol"
      let entry = LogEntry "10" "123" (posixSecondsToUTCTime 1449144737) "keiths-macbook-pro.local" "logplex-parse" "420" "-" [StructuredData "foo" [("bar", "\\\"baz")]] (Just "troll=lolol")
      logEntries `shouldBe` Right [entry, entry]

    it "should parse example heroku documents" $ do
      -- this doesn't actually conform to the BNF in <https://tools.ietf.org/html/rfc5424#section-6>
      -- because STRUCTURED-DATA is defined as a NILVALUE or at least one
      -- SD-ELEMENT neither of which are present here.
      let logEntries = parseLogplex "277 <158>1 2015-12-13T06:02:17.590861+00:00 host heroku router - at=error code=H10 desc=\"App crashed\" method=POST path=\"/crash\" host=ancient-savannah-2923.herokuapp.com request_id=fdcf3bf3-97d3-48a5-8eff-3b28789e36ee fwd=\"120.148.233.212\" dyno= connect= service= status=503 bytes=\n"
      let entry = LogEntry "158" "1" (posixSecondsToUTCTime 1449986537.590861) "host" "heroku" "router" "-" [] (Just "at=error code=H10 desc=\"App crashed\" method=POST path=\"/crash\" host=ancient-savannah-2923.herokuapp.com request_id=fdcf3bf3-97d3-48a5-8eff-3b28789e36ee fwd=\"120.148.233.212\" dyno= connect= service= status=503 bytes=\n")
      logEntries `shouldBe` Right [entry]
