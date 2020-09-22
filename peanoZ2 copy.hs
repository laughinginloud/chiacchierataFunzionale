{-# LANGUAGE LambdaCase #-}

module PeanoZ where

import Prelude hiding (Int)

data Int = Z | S Int | P Int
    deriving (Show, Read, Eq)

normalize :: Int -> Int
normalize = \case
    Z        -> Z
    P (S x') -> normalize x'
    S (P x') -> normalize x'
    P x'     -> P (normalize x')
    S x'     -> S (normalize x')
        
instance Ord Int where
    compare x y =
        case (x, y) of
            (P x', P y') -> compare x' y'
            (P _, _)     -> LT
            (_, P _)     -> GT
            (Z, Z)       -> EQ
            (Z, _)       -> LT
            (_, Z)       -> GT
            (S x', S y') -> compare x' y'

instance Num Int where
    (+) x y =
        normalize
        (
            case (x, y) of
                (_, Z)    -> x
                (_, P y') -> P ((+) x y')
                (_, S y') -> S ((+) x y')
        )

    (*) x y =
        normalize
        (
            case (x, y) of
                (_, Z)     -> Z
                (_, S Z)   -> x
                (P _, P _) -> (*) (negate x) (negate y)
                (P _, _)   -> negate ((*) (negate x) y)
                (_, P _)   -> negate ((*) x (negate y))
                (_, S y')  -> (+) ((*) x y') x
        )

    abs =
        (\case
            x@(P _) -> negate x
            x       -> x
        ) . normalize

    signum =
        (\case
            Z   -> 0
            P _ -> -1
            _   -> 1
        ) . normalize

    fromInteger x
        | x == 0    = Z
        | x < 0     = negate (fromInteger (abs x))
        | otherwise = S (fromInteger (x - 1))

    negate = 
        (\case
            Z   -> Z
            P x -> S (negate x)
            S x -> P (negate x)
        ) . normalize

instance Enum Int where
    toEnum = fromInteger . toInteger

    fromEnum = \case
        Z   -> 0
        P x -> (fromEnum x) - 1
        S x -> (fromEnum x) + 1

iperoperazione :: (Num a, Eq a) => a -> a -> a -> a
iperoperazione n a b =
    case (n, b) of
        (0, _) -> b + 1
        (1, 0) -> a
        (2, 0) -> 0
        (_, 0) -> 1
        (_, _) -> iperoperazione (n - 1) a (iperoperazione n a (b - 1))