{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smile.Core
    ( Core (..)
    ) where

import RIO.Process (HasProcessContext (..), ProcessContext)
import Smile.Options (CoreOptions)
import Smile.Prelude

data Core = Core
    { logFunc :: !LogFunc
    , processContext :: !ProcessContext
    , options :: !CoreOptions
    } deriving (Generic)

instance Has Core r => Has LogFunc r where
    hasLens = hasLens . (field @"logFunc" :: Lens' Core LogFunc)

instance Has Core r => Has ProcessContext r where
    hasLens = hasLens . (field @"processContext" :: Lens' Core ProcessContext)

instance Has LogFunc r => HasLogFunc r where
    logFuncL = hasLens

instance Has Core r => HasProcessContext r where
    processContextL = hasLens
