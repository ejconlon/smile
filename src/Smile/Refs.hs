module Smile.Refs where

import Smile.Prelude

have :: Has thing env => RIO env thing
have = getter <$> ask

haveLens :: Has thing env => Lens' thing b -> RIO env b
haveLens lenz = view (hasLens . lenz)

readRef :: Has thing env => Lens' thing (IORef a) -> RIO env a
readRef lenz = haveLens lenz >>= liftIO . readIORef

writeRef :: Has thing env => Lens' thing (IORef a) -> a -> RIO env ()
writeRef lenz value = haveLens lenz >>= liftIO . flip writeIORef value

modifyRef :: Has thing env => Lens' thing (IORef a) -> (a -> a) -> RIO env ()
modifyRef lenz f = haveLens lenz >>= liftIO . flip modifyIORef f
