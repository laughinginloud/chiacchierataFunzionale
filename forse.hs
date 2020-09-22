module Forse (Forse(..)) where

import Control.Applicative (Alternative(empty, (<|>)))
import Control.Monad (MonadPlus(mzero), liftM2)
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Zip (MonadZip(mzipWith))

data Forse a = Nulla | Solo a
    deriving (Show, Read, Eq, Ord)

instance Semigroup a => Semigroup (Forse a) where
    Solo x <> Solo y = Solo (x <> y)
    _ <> _ = Nulla

instance Monoid a => Monoid (Forse a) where
    mempty = Solo mempty

instance Functor Forse where
    fmap f (Solo x) = Solo (f x)
    fmap _ _ = Nulla

instance Applicative Forse where
    pure x = Solo x

    Solo f <*> Solo x = Solo (f x)
    _ <*> _ = Nulla

instance Monad Forse where
    Solo x >>= f = f x
    _ >>= _ = Nulla

instance Alternative Forse where
    empty = Nulla

    Nulla <|> x = x
    x <|> _ = x

instance MonadPlus Forse

instance MonadFail Forse where
    fail _ = mzero

instance MonadZip Forse where
    mzipWith = liftM2

instance MonadFix Forse where
    mfix f = let a = f (ext a) in a
        where
            ext (Solo x) = x
            ext _ = error "Errore: Nulla"

instance Foldable Forse where
    foldMap f (Solo x) = f x
    foldMap _ _ = mempty

instance Traversable Forse where
    traverse f (Solo x) = Solo <$> f x
    traverse _ _ = pure Nulla