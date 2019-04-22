module Smile.Refs where

import Smile.Prelude

have :: (MonadReader env m, Has thing env) => Lens' thing b -> m b
have lenz = view (hasLens . lenz)

readRef :: (MonadIO m, MonadReader env m, Has thing env) => Lens' thing (IORef a) -> m a
readRef lenz = have lenz >>= liftIO . readIORef

writeRef :: (MonadIO m, MonadReader env m, Has thing env) => Lens' thing (IORef a) -> a -> m ()
writeRef lenz value = have lenz >>= liftIO . flip writeIORef value

modifyRef :: (MonadIO m, MonadReader env m, Has thing env) => Lens' thing (IORef a) -> (a -> a) -> m ()
modifyRef lenz f = have lenz >>= liftIO . flip modifyIORef f
