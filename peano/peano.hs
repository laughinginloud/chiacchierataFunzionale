module Peano
(
    Naturale,
    precedente,
    daInt,
    inInt,
    conversioneGen,
    iperoperazione
)
where

-- Definizione del tipo
data Naturale = Zero | Successivo Naturale
    deriving (Show, Read, Eq, Ord)
    
instance Num Naturale where
    (+) = iperoperazione 1
    (-) x y
        | y == 0    = x
        | x >= y    = (-) (precedente x) (precedente y)
        | otherwise = error "La sottrazione non è chiusa rispetto ai naturali"
    (*) = iperoperazione 2
    abs x = x
    signum 0 = 0
    signum x = 1
    fromInteger = daInt
    negate x = error "Non è possibile negare un numero naturale"

instance Enum Naturale where
    toEnum = daInt . toInteger
    fromEnum = fromInteger . inInt
-- -------------------

-- Operazioni di base
precedente :: Naturale -> Naturale
precedente 0 = 0
precedente (Successivo n) = n

type Base = Integer
type Passo = Integer

daIntGen :: Base -> Passo -> Integer -> Naturale
daIntGen xo x n
    | n == xo   = Zero
    | otherwise = Successivo $ daIntGen xo x $ n - x

inIntGen :: Base -> Passo -> Naturale -> Integer
inIntGen xo x n
    | n == (daIntGen xo x xo) = xo
    | otherwise               = x + (inIntGen xo x (precedente n))

conversioneGen :: Base -> Passo -> (Integer -> Naturale, Naturale -> Integer)
conversioneGen xo x = (daIntGen xo x, inIntGen xo x)

daInt :: Integer -> Naturale
inInt :: Naturale -> Integer
(daInt, inInt) = conversioneGen 0 1
-- -------------------

-- Operazioni avanzate
iperoperazione :: (Num a, Eq a) => a -> a -> a -> a
iperoperazione n a b =
    case (n, b) of
        (0, _) -> b + 1
        (1, 0) -> a
        (2, 0) -> 0
        (_, 0) -> 1
        (_, _) -> iperoperazione (n - 1) a (iperoperazione n a (b - 1))
-- -------------------

sum1 x Zero = x
sum1 x (Successivo y) = Successivo (sum1 x y)