{-# LANGUAGE UndecidableInstances #-}

module Smile.Stats
    ( Counter
    , HasStore (..)
    , Store
    , forkServer
    , newCounter
    , registerCounter
    , incCounter
    , readCounter
    ) where

import Smile.Prelude
import System.Metrics (Store)
import System.Metrics.Counter (Counter)
import qualified System.Metrics.Counter as C
import qualified System.Metrics as M
import System.Remote.Monitoring (forkServerWith)

class HasStore env where
    storeLens :: Lens' env Store

forkServer :: HasStore env => Text -> Int -> RIO env ()
forkServer host port = do
    store <- view storeLens
    _ <- liftIO (forkServerWith store (encodeUtf8 host) port)
    pure ()

newCounter :: IO Counter
newCounter = C.new

registerCounter :: HasStore env => Text -> Lens' env Counter -> RIO env ()
registerCounter name lenz = do
    store <- view storeLens
    v <- view lenz
    liftIO (M.registerCounter name (C.read v) store)

incCounter :: Lens' env Counter -> RIO env ()
incCounter lenz = view lenz >>= liftIO . C.inc

readCounter :: Lens' env Counter -> RIO env Int64
readCounter lenz = view lenz >>= liftIO . C.read

-- All stats must start with APPLICATION_ENV.app.NAME
-- Statsd based interface
-- env vars and defaults:
-- APPLICATION_ENV = development
-- STATSD_HOST = 127.0.0.1
-- STATSD_PORT = 8125
-- expose http port with http://hackage.haskell.org/package/ekg
-- statsd version with http://hackage.haskell.org/package/ekg-statsd
-- core functions with http://hackage.haskell.org/package/ekg-core
