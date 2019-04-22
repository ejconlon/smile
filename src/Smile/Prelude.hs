module Smile.Prelude
    ( module RIO
    , Has (..)
    , makeSmileLenses
    ) where

import Control.Lens ((.~))
import Control.Lens.TH (DefName (TopName), lensField, lensRules, makeLensesWith)
import Data.Has (Has (..))
import RIO
import Language.Haskell.TH (DecsQ, Name, mkName, nameBase)

makeSmileLenses :: Name -> DecsQ
makeSmileLenses = makeLensesWith $ lensRules
    & lensField .~ \_ _ name -> [TopName (mkName (nameBase name ++ "Lens"))]
