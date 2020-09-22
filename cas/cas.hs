{-# LANGUAGE GADTs, DataKinds, KindSignatures #-}

module CAS where

-- import Data.Tree

data Termine :: * -> * where
    N :: Num a => a -> Termine a
    F :: Num a => ([a] -> a) -> Termine ([a] -> a)
    --deriving Show

data Espressione :: * -> * where
    Nodo :: Termine a -> [Espressione (Termine a)] -> Espressione (Termine a)
    --deriving Show

--data Espressione a = Tree (Termine a)

--data Albero a = Niet | El a [Albero a]

eval :: Num a => Espressione b -> a
eval (Nodo (N x) _) = x
eval (Nodo (F g) ls) = g (map eval ls)