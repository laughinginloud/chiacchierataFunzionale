{-# LANGUAGE LambdaCase #-}

module PeanoZ where

import Prelude hiding (Int)

data Int = Z | S Int | N Int
    deriving (Show, Read)

instance Eq Int where
    (==) x y =
        case (x, y) of
            (Z, Z)       -> True
            (S x', S y') -> (==) x' y'
            (N Z, Z)     -> True
            (Z, N Z)     -> True
            (_, _)       -> False

normalize :: Int -> Int
normalize = \case
    Z           -> Z
    N Z         -> Z
    N (N x)     -> normalize x
    N x         -> negate (normalize x)
    S (N (S x)) -> normalize (negate (normalize x))
    S x         -> S (normalize x)

instance Ord Int where
    compare x y =
        case (x, y) of
            (N x', N y') -> compare x' y'
            (N _, _)     -> LT
            (_, N _)     -> GT
            (Z, Z)       -> EQ
            (Z, _)       -> LT
            (_, Z)       -> GT
            (S x', S y') -> compare x' y'

instance Num Int where
    (+) x y =
        normalize
        (
            case (x, y) of
                (_, Z)           -> x
                (N x', N y')     -> negate ((+) x' y')
                (N (S x'), S y') -> (+) (negate x') y'
                (S x', N (S y')) -> (+) x' (negate y')
                (_, S y')        -> (+) (S x) y'
        )

    (*) x y =
        normalize
        (
            case (x, y) of
                (_, Z)       -> Z
                (_, S Z)     -> x
                (N x', N y') -> (*) x' y'
                (N x', _)    -> negate ((*) x' y)
                (_, N y')    -> negate ((*) x y')
                (_, S y')    -> (+) ((*) x y') x
        )

    abs =
        (\case
            N x -> x
            x   -> x
        ) . normalize

    signum =
        (\case
            Z   -> 0
            N _ -> -1
            _   -> 1
        ) . normalize

    fromInteger x
        | x == 0    = Z
        | x < 0     = N (fromInteger (abs x))
        | otherwise = S (fromInteger (x - 1))

    negate =
        (\case
            Z   -> Z
            N x -> x
            x   -> N x
        ) . normalize

instance Enum Int where
    toEnum = fromInteger . toInteger

    fromEnum = \case
        Z   -> 0
        N x -> negate (fromEnum x)
        S x -> (fromEnum x) + 1

iperoperazione :: (Num a, Eq a) => a -> a -> a -> a
iperoperazione n a b =
    case (n, b) of
        (0, _) -> b + 1
        (1, 0) -> a
        (2, 0) -> 0
        (_, 0) -> 1
        (_, _) -> iperoperazione (n - 1) a (iperoperazione n a (b - 1))