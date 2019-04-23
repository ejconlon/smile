{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Smile.Stats where

import Smile.Prelude
import Smile.Refs (haveLens)
import System.Metrics (Store)
import qualified System.Metrics.Counter as C
import qualified System.Metrics as M

class HasStore env where
    storeL :: Lens' env Store

instance Has Store env => HasStore env where
    storeL = hasLens

registerCounter :: (HasStore env, Has thing env) => Text -> Lens' thing C.Counter -> RIO env ()
registerCounter name lenz = do
    store <- view storeL
    v <- haveLens lenz
    liftIO (M.registerCounter name (C.read v) store)

incCounter :: Has thing env => Lens' thing C.Counter -> RIO env ()
incCounter lenz = haveLens lenz >>= liftIO . C.inc

readCounter :: Has thing env => Lens' thing C.Counter -> RIO env Int64
readCounter lenz = haveLens lenz >>= liftIO . C.read

-- All stats must start with APPLICATION_ENV.app.NAME
-- Statsd based interface
-- env vars and defaults:
-- APPLICATION_ENV = development
-- STATSD_HOST = 127.0.0.1
-- STATSD_PORT = 8125
-- expose http port with http://hackage.haskell.org/package/ekg
-- statsd version with http://hackage.haskell.org/package/ekg-statsd
-- core functions with http://hackage.haskell.org/package/ekg-core
