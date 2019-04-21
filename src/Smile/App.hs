module Smile.App
    ( App
    , appRestL
    , mkApp
    ) where

import Smile.Core (Core)
import Smile.Prelude

data App a = App
    { rest :: a
    , core :: Core
    } deriving (Generic)

mkApp :: a -> Core -> App a
mkApp = App

appCoreL :: Lens' (App a) Core
appCoreL = field @"core"

appRestL :: Lens' (App a) a
appRestL = field @"rest"

instance Has Core (App a) where
    hasLens = appCoreL
