module Smile.Refs where

import Smile.Prelude

readRef :: (MonadIO m, MonadReader env m) => Lens' env (IORef a) -> m a
readRef lenz = do
  ref <- view lenz
  liftIO (readIORef ref)
