{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smile.Core where

import RIO.Process   (HasProcessContext (..), ProcessContext)
import Smile.Options (CoreOptions)
import Smile.Prelude
import System.Metrics

data Core = Core
    { _logFunc        :: !LogFunc
    , _processContext :: !ProcessContext
    , _store          :: !Store
    , _options        :: !CoreOptions
    } deriving (Generic)

$(makeSmileLenses ''Core)

instance Has Core r => Has LogFunc r where
    hasLens = hasLens . _logFuncLens

instance Has Core r => Has ProcessContext r where
    hasLens = hasLens . _processContextLens

instance Has Core r => Has Store r where
    hasLens = hasLens . _storeLens

instance Has Core r => Has CoreOptions r where
    hasLens = hasLens . _optionsLens

instance Has LogFunc r => HasLogFunc r where
    logFuncL = hasLens

instance Has Core r => HasProcessContext r where
    processContextL = hasLens
