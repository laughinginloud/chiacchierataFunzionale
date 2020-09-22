module Liste where

import Control.Applicative (Alternative(empty, (<|>)), liftA2)
import Control.Monad (MonadPlus, liftM2)
import Control.Monad.Fix (MonadFix(..), fix)
import Control.Monad.Zip (MonadZip(mzipWith))

data Lista a = Nil | El a (Lista a)
    deriving (Show, Read, Eq, Ord)

instance Semigroup (Lista a) where
    (El x xs) <> y = El x (xs <> y)
    _ <> x = x

instance Monoid (Lista a) where
    mempty = Nil

instance Functor Lista where
    fmap f (El x xs) = El (f x) (fmap f xs)
    fmap _ _ = Nil

instance Applicative Lista where
    pure x = El x Nil

    (El f fs) <*> xs = (fmap f xs) <> (fs <*> xs)
    _ <*> _ = Nil

instance Monad Lista where
    (El x xs) >>= f = (f x) <> (xs >>= f)
    _ >>= _ = Nil

instance Alternative Lista where
    empty = Nil

    (<|>) = (<>)

instance MonadPlus Lista

instance MonadFail Lista where
    fail _ = Nil

instance MonadZip Lista where
    mzipWith = liftM2

instance MonadFix Lista where
    mfix f =
        case fix (f . testa) of
            Nil      -> Nil
            (El x _) -> El x (mfix (coda . f))
        where
            testa (El x _) = x
            coda (El _ xs) = xs

instance Foldable Lista where
    foldMap f (El x xs) = (f x) <> (foldMap f xs)
    foldMap _ _ = mempty

instance Traversable Lista where
    traverse f = foldr el (pure Nil)
        where el x ys = liftA2 El (f x) ys


inserimentoInTesta :: a -> Lista a -> Lista a
inserimentoInTesta x ls = El x ls

inserimentoInCoda :: a -> Lista a -> Lista a
inserimentoInCoda x Nil = El x Nil
inserimentoInCoda x (El y ls) = El y (inserimentoInCoda x ls)

rimozioneInTesta :: Lista a -> Lista a
rimozioneInTesta Nil = Nil
rimozioneInTesta (El _ ls) = ls

rimozioneInCoda :: Lista a -> Lista a
rimozioneInCoda Nil = Nil
rimozioneInCoda (El _ Nil) = Nil
rimozioneInCoda (El x ls) = El x (rimozioneInCoda ls)

rimozione :: Eq a => a -> Lista a -> Lista a
rimozione _ Nil = Nil
rimozione x (El y ls)
    | x == y    = rimozione x ls
    | otherwise = El y (rimozione x ls)

lunghezza :: Lista a -> Integer
lunghezza Nil = 0
lunghezza (El _ ls) = 1 + lunghezza ls

isPresente :: Eq a => a -> Lista a -> Bool
isPresente _ Nil = False
isPresente x (El y ls)
    | x == y    = True
    | otherwise = isPresente x ls

stampa :: Show a => Lista a -> IO ()
stampa Nil = putStrLn "Null"
stampa (El x ls) = (putStr . (\x -> x ++ "->") . show) x >> stampa ls