module Smile.App where

import RIO.Process (HasProcessContext (..), ProcessContext, mkDefaultProcessContext)
import Smile.Prelude

data Options = Options
  { optionsVerbose :: !Bool
  } deriving (Generic)

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  } deriving (Generic)

instance HasLogFunc App where
  logFuncL = field @"appLogFunc"

instance HasProcessContext App where
  processContextL = field @"appProcessContext"

class HasApp env where
  appL :: Lens' env App

type LogC env m = (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)

exe :: Options -> (App -> IO a) -> IO a
exe options body = do
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          }
    in body app
