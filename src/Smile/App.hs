{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smile.App where

import Smile.Core    (Core, HasCore (..))
import Smile.Prelude

data App a = App
    { _rest :: a
    , _core :: Core
    }

$(makeSmileLenses ''App)

instance HasApp r a => HasCore r where
    coreLens = appLens . coreField
