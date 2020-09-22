{-# LANGUAGE LambdaCase #-}

module PeanoZ where

data Intero = Zero | Successivo Intero | Negazione Intero
    deriving (Show, Read, Eq)

{-
instance Eq Intero where
    (==) x y =
        case (x, y) of
            (Zero, Zero)                   -> True
            (Successivo x', Successivo y') -> (==) x' y'
            (Negazione Zero, Zero)         -> True
            (Zero, Negazione Zero)         -> True
            (_, _)                         -> False
-}

normalizza :: Intero -> Intero
normalizza = \case
    Zero                                  -> Zero
    Negazione Zero                        -> Zero
    Negazione (Negazione x)               -> normalizza x
    Negazione x                           -> negate (normalizza x)
    Successivo (Negazione (Successivo x)) -> normalizza (negate (normalizza x))
    Successivo x                          -> Successivo (normalizza x)

instance Ord Intero where
    compare x y =
        case (x, y) of
            (Negazione x', Negazione y')    -> compare x' y'
            (Negazione _, _)                -> LT
            (_, Negazione _)                -> GT
            (Zero, Zero)                    -> EQ
            (Zero, _)                       -> LT
            (_, Zero)                       -> GT
            (Successivo x', Successivo y')  -> compare x' y'

instance Num Intero where
    (+) x y =
        normalizza
        (
            case (x, y) of
                (_, Zero)                                  -> x
                (Negazione x', Negazione y')               -> negate ((+) x' y')
                (Negazione (Successivo x'), Successivo y') -> (+) (negate x') y'
                (Successivo x', Negazione (Successivo y')) -> (+) x' (negate y')
                (_, Successivo y')                         -> (+) (Successivo x) y'
        )

    (*) x y =
        normalizza
        (
            case (x, y) of
                (_, Zero)                    -> Zero
                (_, Successivo Zero)         -> x
                (Negazione x', Negazione y') -> (*) x' y'
                (Negazione x', _)            -> negate ((*) x' y)
                (_, Negazione y')            -> negate ((*) x y')
                (_, Successivo y')           -> (+) ((*) x y') x
        )

    abs =
        (\case
            Negazione x -> x
            x           -> x
        ) . normalizza

    signum =
        (\case
            Zero        -> 0
            Negazione _ -> -1
            _           -> 1
        ) . normalizza

    fromInteger x
        | x == 0    = Zero
        | x < 0     = Negazione (fromInteger (abs x))
        | otherwise = Successivo (fromInteger (x - 1))

    negate =
        (\case
            Zero        -> Zero
            Negazione x -> x
            x           -> Negazione x
        ) . normalizza

instance Enum Intero where
    toEnum = fromInteger . toInteger

    fromEnum = \case
        Zero         -> 0
        Negazione x  -> negate (fromEnum x)
        Successivo x -> (fromEnum x) + 1

iperoperazione :: (Num a, Eq a) => a -> a -> a -> a
iperoperazione n a b =
    case (n, b) of
        (0, _) -> b + 1
        (1, 0) -> a
        (2, 0) -> 0
        (_, 0) -> 1
        (_, _) -> iperoperazione (n - 1) a (iperoperazione n a (b - 1))