-- | Fun inspired by Selective
-- http://hackage.haskell.org/package/selective
module Smile.Cli.Selective where

import Data.Functor.Compose
import GHC.Exts (Constraint)
import Smile.Prelude
import qualified Data.Map as Map

-- | Something weaker than `Profunctor g`, like `forall x. Functor (g x)`
-- I don't know if it already exists somewhere, but it's easy enough to define it here.
class NatFunctor (g :: * -> * -> *) where
    nrmap :: (b -> c) -> g a b -> g a c

instance NatFunctor ((->)) where
    nrmap = (.)

-- | Something we can `<*>`
newtype Fun f a b = Fun { unFun :: Compose f ((->) a) b } deriving (Functor, Applicative)

outFun :: Fun f a b -> f (a -> b)
outFun = getCompose . unFun

inFun :: f (a -> b) -> Fun f a b
inFun = Fun . Compose

instance Functor f => NatFunctor (Fun f) where
    nrmap x = inFun . fmap (nrmap x) . outFun

-- | This is the class I want to write, but the type checker isn't happy using it:
--
-- class Selective (k :: (* -> *) -> Constraint) (c :: * -> Constraint)  (g :: * -> * -> *) (f :: * -> *) where
--     select :: (k m, c a) => (forall x. f x -> m x) -> f a -> g a b -> m b
--
-- To work around it, I am newtyping a la https://github.com/snowleopard/build/blob/master/src/Build/Task.hs
--
-- Basically the natural transformation controls our interpretation of effects. This class contains the
-- strategy of "unwrapping" the `a` from an `f a` and checking it against some known group of `a` in `g a b`.
-- Sounds obscure, but this is really just abstractly defining statically analyzable branches of a parser
-- for the right `f` and `g`. The constraints `k` on the monad we interpret into and `c` on the `a` branch we
-- analyze are there to ensure we can use those types in the ways our concrete `g` choice requires.
--
-- Referring back to S6.1 of the paper, this allows you to embed any multi-way choice operator like their
--
-- `bindS :: Selective f => (Bounded a, Enum a, Eq a) => f a -> (a -> f b) -> f b`
--
newtype Selector (k :: (* -> *) -> Constraint) (c :: * -> Constraint) (g :: * -> * -> *) (f :: * -> *) =
    Selector { unSelector :: forall (m :: * -> *) (a :: *) (b :: *). (k m, c a) => (forall x. f x -> m x) -> f a -> g a b -> m b }

-- | Jacked from http://hackage.haskell.org/package/trivial-constraint
class Unconstrained t
instance Unconstrained t

appSelector :: Selector Applicative Unconstrained (Fun f) f
appSelector = Selector $ \nat x y -> nat (outFun y) <*> nat x

-- | This is a really complicated reimplementation of `flip (<*>)`.
runAppSelector :: Applicative m => (forall x. f x -> m x) -> f a -> f (a -> b) -> m b
runAppSelector nat x y = (unSelector appSelector) nat x (inFun y)

newtype DefName = DefName { unDefName :: Text } deriving (Eq, Show, Display, Ord, IsString)

data DefMap f a b = DefMap
    { _defMapName :: DefName
    , _defMapChoices :: Map a (f b)
    }

instance Functor f => NatFunctor (DefMap f) where
    nrmap f (DefMap n c) = DefMap n (fmap (fmap f) c)

$(makeSmileLenses ''DefMap)

-- | This is a more interesting one. Your free Selective structure can
-- embed choices in `Map`s (hence the `Ord` constraint on keys) that we
-- can statically analyze. We bail out of interpretation on missing keys
-- (hence the `MonadError` constraint on the interpretation monad).
defMapSelector :: Selector (MonadError DefName) Ord (DefMap f) f
defMapSelector = Selector $ \nat x (DefMap n y) -> do
    a <- nat x
    case Map.lookup a y of
        Nothing -> throwError n
        Just z -> nat z

-- | Not a direct encoding of a free Selective; instead has some knots Selector will untie.
data FreeSelective (c :: * -> Constraint) (g :: * -> * -> *) (f :: * -> *) (a :: *) where
    Pure :: a -> FreeSelective c g f a
    Ap :: f r -> FreeSelective c g f (r -> a) -> FreeSelective c g f a
    Sel :: c r => f r -> g r (FreeSelective c g f a) -> FreeSelective c g f a

instance NatFunctor g => Functor (FreeSelective c g f) where
    fmap f s =
        case s of
            Pure a -> Pure (f a)
            Ap x y -> Ap x (fmap (fmap f) y)
            Sel x y -> Sel x (nrmap (fmap f) y)

instance NatFunctor g => Applicative (FreeSelective c g f) where
    pure = Pure
    Pure f <*> y = fmap f y
    Ap x y <*> z = Ap x (flip <$> y <*> z)
    Sel x y <*> z = Sel x (nrmap (<*> z) y)

-- | Bringing it all back home: interpreting the free Selective given a strategy to combine
-- `f`s and `g`s and a natural transformation to some `Monad` to sequence just the effects we choose.
interp :: (Monad m, k m) => Selector k c g f -> (forall x. f x -> m x) -> FreeSelective c g f a -> m a
interp _ _ (Pure a) = pure a
interp s nat (Ap x y) = interp s nat y <*> nat x
interp s nat (Sel x y) = (unSelector s) nat x y >>= interp s nat
