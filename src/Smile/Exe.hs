module Smile.Exe
  ( exe
  , exeBlank
  ) where

import Smile.EnvParser     (EnvParser, emptyEnvParser, getAndRunEnvParser)
import RIO.Process         (mkDefaultProcessContext)
import Smile.Core          (Core (..))
import Smile.Options       (CoreOptions (..), Options (..), optionsParser)
import Smile.Prelude
import System.Metrics      (newStore, registerGcMetrics)

innerExe :: CoreOptions -> (Core -> IO c) -> IO c
innerExe opts body = do
  lo <- logOptionsHandle stderr (_verbose opts)
  pc <- mkDefaultProcessContext
  s <- newStore
  when (_gcMetrics opts) (registerGcMetrics s)
  withLogFunc lo $ \lf ->
    let core = Core
          { _logFunc = lf
          , _processContext = pc
          , _store = s
          , _options = opts
          }
    in body core

exe :: EnvParser a -> (a -> Core -> IO b) -> (b -> IO c) -> IO c
exe parser prepare run = do
  opts <- getAndRunEnvParser (optionsParser parser)
  innerExe (_coreOptions opts) $ \core -> do
    b <- prepare (_appOptions opts) core
    run b

exeBlank :: (Core -> IO c) -> IO c
exeBlank = exe emptyEnvParser (\_ c -> pure c)
