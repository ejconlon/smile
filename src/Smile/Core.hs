{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smile.Core where

import RIO.Process   (HasProcessContext (..), ProcessContext)
import Smile.Options (CoreOptions)
import Smile.Prelude
import Smile.Stats   (HasStore (..), Store)

data Core = Core
    { _logFunc        :: !LogFunc
    , _processContext :: !ProcessContext
    , _store          :: !Store
    , _options        :: !CoreOptions
    } deriving (Generic)

$(makeSmileLenses ''Core)

instance HasCore r => HasLogFunc r where
    logFuncL = coreLens . logFuncField

instance HasCore r => HasProcessContext r where
    processContextL = coreLens . processContextField

instance HasCore r => HasStore r where
    storeLens = coreLens . storeField
