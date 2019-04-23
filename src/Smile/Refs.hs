module Smile.Refs where

import Smile.Prelude

readRef :: Lens' env (IORef a) -> RIO env a
readRef lenz = view lenz >>= liftIO . readIORef

writeRef :: Lens' env (IORef a) -> a -> RIO env ()
writeRef lenz value = view lenz >>= liftIO . flip writeIORef value

modifyRef :: Lens' env (IORef a) -> (a -> a) -> RIO env ()
modifyRef lenz f = view lenz >>= liftIO . flip modifyIORef f
