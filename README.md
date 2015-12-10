# logplex-parse

Parse the Heroku application/logplex documents sent over HTTPS log drains and their syslog contents

## Parsers

Heroku’s Logplex can be [configured](https://devcenter.heroku.com/articles/log-drains) to send batches
of logs over [HTTPS](https://devcenter.heroku.com/articles/log-drains#https-drains). 
The `application/logplex-1` documents use the framing scheme specified in [RFC6587](https://tools.ietf.org/html/rfc6587#section-3.4.1).

Each frame contains a syslog entry as specified in [RFC5424](http://tools.ietf.org/html/rfc5424#section-6).

This module contains a parser for each format and decodes them into a `LogEntry` for consumption by
applications.
