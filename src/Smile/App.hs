{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smile.App where

import Smile.Core    (Core, HasCore (..))
import Smile.Prelude

data App m d = App
    { _core :: Core
    , _metrics :: m
    , _domain :: d
    }

$(makeSmileLenses ''App)

instance HasApp r m d => HasCore r where
    coreLens = appLens . coreField
