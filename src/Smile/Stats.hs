module Smile.Stats where

-- All stats must start with APPLICATION_ENV.app.NAME
-- Statsd based interface
-- env vars and defaults:
-- APPLICATION_ENV = development
-- STATSD_HOST = 127.0.0.1
-- STATSD_PORT = 8125
-- expose http port with http://hackage.haskell.org/package/ekg
-- statsd version with http://hackage.haskell.org/package/ekg-statsd
-- core functions with http://hackage.haskell.org/package/ekg-core
