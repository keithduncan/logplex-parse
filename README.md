# logplex-parse

Parse the Heroku application/logplex documents sent over HTTPS log drains and their syslog contents

## Parsers

Heroku’s Logplex can be [configured](https://devcenter.heroku.com/articles/log-drains) to send batches
of logs over [HTTPS](https://devcenter.heroku.com/articles/log-drains#https-drains).
The `application/logplex-1` documents use the framing scheme specified in [RFC6587](https://tools.ietf.org/html/rfc6587#section-3.4.1).

Each frame contains a syslog entry as specified in [RFC5424](http://tools.ietf.org/html/rfc5424#section-6)
although Heroku forgoes encoding the structured-data nilvalue and skips straight
to the message data ¯\\(°\_o)/¯

This module contains a parser for each format and decodes them into a `LogEntry` for consumption by
applications.

## Examples

```haskell
parseLogplex "106 <10>123 2015-12-03T23:12:17+11:00 keiths-macbook-pro.local logplex-parse 420 - [foo bar=\"baz\"] error=1234595 <10>123 2015-12-03T23:12:17+11:00 keiths-macbook-pro.local logplex-parse 420 - Error R99 heroku95 <10>123 2015-12-03T23:12:17+11:00 keiths-macbook-pro.local logplex-parse 420 - Error R89 heroku95 <10>123 2015-12-03T23:12:17+11:00 keiths-macbook-pro.local logplex-parse 420 - Error L99 heroku120 <10>123 2015-12-03T23:12:17+11:00 keiths-macbook-pro.local logplex-parse 420 - at=error code=H12 desc=\"a thing happened\""

Right [
  LogEntry {getPriority = "10", getVersion = "123", getTimestamp = 2015-12-03 12:12:17 UTC, getHostname = "keiths-macbook-pro.local", getAppname = "logplex-parse", getProcessId = "420", getMessageId = "-", getStructuredData = [StructuredData {getId = "foo", getParams = [("bar","baz")]}], getMessage = Just "error=12345"},
  LogEntry {getPriority = "10", getVersion = "123", getTimestamp = 2015-12-03 12:12:17 UTC, getHostname = "keiths-macbook-pro.local", getAppname = "logplex-parse", getProcessId = "420", getMessageId = "-", getStructuredData = [], getMessage = Just "Error R99 heroku"},
  LogEntry {getPriority = "10", getVersion = "123", getTimestamp = 2015-12-03 12:12:17 UTC, getHostname = "keiths-macbook-pro.local", getAppname = "logplex-parse", getProcessId = "420", getMessageId = "-", getStructuredData = [], getMessage = Just "Error R89 heroku"},
  LogEntry {getPriority = "10", getVersion = "123", getTimestamp = 2015-12-03 12:12:17 UTC, getHostname = "keiths-macbook-pro.local", getAppname = "logplex-parse", getProcessId = "420", getMessageId = "-", getStructuredData = [], getMessage = Just "Error L99 heroku"},
  LogEntry {getPriority = "10", getVersion = "123", getTimestamp = 2015-12-03 12:12:17 UTC, getHostname = "keiths-macbook-pro.local", getAppname = "logplex-parse", getProcessId = "420", getMessageId = "-", getStructuredData = [], getMessage = Just "at=error code=H12 desc=\"a thing happened\""}
]
```
