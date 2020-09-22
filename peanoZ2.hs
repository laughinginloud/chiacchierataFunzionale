{-# LANGUAGE LambdaCase #-}

module PeanoZ where

data Intero = Zero | Successivo Intero | Precedente Intero
    deriving (Show, Read, Eq)

normalizza :: Intero -> Intero
normalizza = \case
    Zero                       -> Zero
    Precedente (Successivo x') -> normalizza x'
    Successivo (Precedente x') -> normalizza x'
    Precedente x'              -> Precedente (normalizza x')
    Successivo x'              -> Successivo (normalizza x')
        
instance Ord Intero where
    compare x y =
        case (x, y) of
            (Precedente x', Precedente y') -> compare x' y'
            (Precedente _, _)              -> LT
            (_, Precedente _)              -> GT
            (Zero, Zero)                   -> EQ
            (Zero, _)                      -> LT
            (_, Zero)                      -> GT
            (Successivo x', Successivo y') -> compare x' y'

instance Num Intero where
    (+) x y =
        normalizza
        (
            case (x, y) of
                (_, Zero)          -> x
                (_, Precedente y') -> (Precedente ((+) x y'))
                (_, Successivo y') -> (Successivo ((+) x y'))
        )

    (*) x y =
        normalizza
        (
            case (x, y) of
                (_, Zero)                    -> Zero
                (_, Successivo Zero)         -> x
                (Precedente _, Precedente _) -> (*) (negate x) (negate y)
                (Precedente _, _)            -> negate ((*) (negate x) y)
                (_, Precedente _)            -> negate ((*) x (negate y))
                (_, Successivo y')           -> (+) ((*) x y') x
        )

    abs =
        (\case
            x@(Precedente _) -> negate x
            x                -> x
        ) . normalizza

    signum =
        (\case
            Zero         -> 0
            Precedente _ -> -1
            _            -> 1
        ) . normalizza

    fromInteger x
        | x == 0    = Zero
        | x < 0     = negate (fromInteger (abs x))
        | otherwise = Successivo (fromInteger (x - 1))

    negate = 
        (\case
            Zero         -> Zero
            Precedente x -> Successivo (negate x)
            Successivo x -> Precedente (negate x)
        ) . normalizza

instance Enum Intero where
    toEnum = fromInteger . toInteger

    fromEnum = fromInteger . toInteger

instance Real Intero where
    toRational = toRational . toInteger

instance Integral Intero where
    quotRem x y = (quot, rem)
        where res = quotRem (toInteger x) (toInteger y)
              quot = fromInteger (fst res)
              rem = fromInteger (snd res)

    toInteger = \case
        Zero         -> 0
        Precedente x -> (toInteger x) - 1
        Successivo x -> (toInteger x) + 1

instance Semigroup Intero where
    (<>) = (+)

instance Monoid Intero where
    mempty = Zero

iperoperazione :: (Num a, Eq a) => a -> a -> a -> a
iperoperazione n a b =
    case (n, b) of
        (0, _) -> b + 1
        (1, 0) -> a
        (2, 0) -> 0
        (_, 0) -> 1
        (_, _) -> iperoperazione (n - 1) a (iperoperazione n a (b - 1))