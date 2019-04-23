module Smile.Prelude
    ( module RIO
    , Has (..)
    , makeSmileLenses
    ) where

import Control.Lens        ((.~))
import Control.Lens.TH     (DefName (TopName), lensField, lensRules, makeLensesWith)
import Data.Has            (Has (..))
import Language.Haskell.TH (DecsQ, Name, mkName, nameBase)
import RIO

makeSmileLenses :: Name -> DecsQ
makeSmileLenses = makeLensesWith $ lensRules
    & lensField .~ (\_ _ name -> [TopName (mkName (nameBase name ++ "Lens"))])
