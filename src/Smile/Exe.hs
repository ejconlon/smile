module Smile.Exe
  ( exe
  , exeBlank
  ) where

import Options.Applicative (Parser, execParser, info)
import RIO.Process (mkDefaultProcessContext)
import Smile.Core (Core (..))
import Smile.Options (CoreOptions (..), Options (..), optionsParser)
import Smile.Prelude

innerExe :: CoreOptions -> (Core -> IO c) -> IO c
innerExe options body = do
  lo <- logOptionsHandle stderr (verbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let core = Core
          { logFunc = lf
          , processContext = pc
          , options = options
          }
    in body core

exe :: Parser a -> (a -> Core -> IO b) -> (b -> IO c) -> IO c
exe parser prepare run = do
  let parserInfo = info (optionsParser parser) mempty
  Options {..} <- execParser parserInfo
  innerExe coreOptions (\core -> prepare appOptions core >>= run)

exeBlank :: (Core -> IO c) -> IO c
exeBlank body = exe (pure ()) (\_ c -> pure c) body
