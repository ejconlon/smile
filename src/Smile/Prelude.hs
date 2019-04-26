module Smile.Prelude
    ( module RIO
    , ExceptT (..)
    , Except
    , MonadError (..)
    , runExceptT
    , StateT (..)
    , State
    , MonadState (..)
    , makeSmileLenses
    ) where

import Control.Monad.Except (ExceptT (..), Except, MonadError (..), runExceptT)
import Control.Monad.State  (StateT (..), State, MonadState (..))
import Control.Lens         ((.~))
import Control.Lens.TH      (DefName (TopName), classyRules, lensClass, lensField, makeLensesWith)
import Data.Char            (toLower)
import Language.Haskell.TH  (DecsQ, Name, mkName, nameBase)
import RIO

makeSmileLenses :: Name -> DecsQ
makeSmileLenses = makeLensesWith $ classyRules
    & lensField .~ (\_ _ n ->
        case nameBase n of
            '_':xs -> [TopName (mkName (xs ++ "Field"))]
            n' -> error ("Field name must start with underscore: " ++ n')
        )
    & lensClass .~ (\n ->
        case nameBase n of
            n'@(x:xs) -> Just (mkName ("Has" ++ n'), mkName ((toLower x:xs) ++ "Lens"))
            n' -> error ("Cannot make Lens class: " ++ n')
        )
