module Smile.Logging where

import Smile.Prelude

type LogR env = (HasLogFunc env, HasCallStack)
