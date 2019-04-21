module Smile.Logging
    ( LogC
    ) where

import Smile.Prelude

type LogC env m = (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
