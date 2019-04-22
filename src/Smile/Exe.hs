module Smile.Exe
  ( exe
  , exeBlank
  ) where

import Options.Applicative (Parser, execParser, info)
import RIO.Process         (mkDefaultProcessContext)
import Smile.Core          (Core (..))
import Smile.Options       (CoreOptions (..), Options (..), optionsParser)
import Smile.Prelude

innerExe :: CoreOptions -> (Core -> IO c) -> IO c
innerExe opts body = do
  lo <- logOptionsHandle stderr (_verbose opts)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let core = Core
          { _logFunc = lf
          , _processContext = pc
          , _options = opts
          }
    in body core

exe :: Parser a -> (a -> Core -> IO b) -> (b -> IO c) -> IO c
exe parser prepare run = do
  let parserInfo = info (optionsParser parser) mempty
  opts <- execParser parserInfo
  innerExe (_coreOptions opts) $ \core -> do
    b <- prepare (_appOptions opts) core
    run b

exeBlank :: (Core -> IO c) -> IO c
exeBlank = exe (pure ()) (\_ c -> pure c)
