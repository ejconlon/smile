module Smile.App where

import Smile.Core    (Core)
import Smile.Prelude

data App a = App
    { _rest :: a
    , _core :: Core
    }

$(makeSmileLenses ''App)

instance Has Core (App a) where
    hasLens = _coreLens
