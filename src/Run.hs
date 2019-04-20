module Run
  ( App (..)
  , Options (..)
  , run
  ) where

import Mylude
import RIO.Process (HasProcessContext (..), ProcessContext)

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  } deriving (Generic)

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  } deriving (Generic)

instance HasLogFunc App where
  logFuncL = field @"appLogFunc"

instance HasProcessContext App where
  processContextL = field @"appProcessContext"

type LogC env m = (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)

run :: LogC env m => m ()
run = do
  logInfo "We're inside the application!"
