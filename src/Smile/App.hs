module Smile.App where

import Smile.Core    (Core, HasCore (..))
import Smile.Prelude

data App m d = App
    { _core :: Core
    , _metrics :: m
    , _domain :: d
    }

$(makeSmileLenses ''App)

instance HasCore (App m d) where
    coreLens = coreField
