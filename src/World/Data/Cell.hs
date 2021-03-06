{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module World.Data.Cell
( module World.Data.Cell
, module Data.Strict.Tuple 
) where

import Data.Strict.Tuple
import Control.DeepSeq

newtype Cell = Cell (Int :!: Int) deriving (Eq,Show,Ord)

type CellLong = Double

cellRow, cellCol :: Cell -> Int
cellRow (Cell (r :!: _)) = r
cellCol (Cell (_ :!: c)) = c

cellOp1 :: (Int -> Int) -> Cell -> Cell
cellOp1 op (Cell (r :!: c)) = Cell((op r):!:(op c))
cellOp2 :: (Int -> Int -> Int) -> Cell -> Cell -> Cell
cellOp2 op (Cell (ar :!: ac)) (Cell (br :!: bc)) = Cell((ar `op` br):!:(ac `op` bc))

instance Num(Cell) where 
    (+) = cellOp2 (+)
    (*) = cellOp2 (*)
    (-) = cellOp2 (-)
    abs = cellOp1 abs
    signum = cellOp1 signum
    fromInteger i = Cell (fromInteger i :!: fromInteger i)

instance NFData Cell where
    rnf (Cell t) = t `seq` ()

cellDistance :: Cell -> Cell -> Int
cellDistance a b = let c = a -b 
                       in max (cellRow c) (cellCol c)

class Convertible a b where
        to :: a -> b
        from :: b -> a
        wrap :: (a -> a) -> b -> b
        wrap f a = to $ f (from a)
        wrap2 :: (a -> a -> a) -> b -> b -> b
        wrap2 f a b = to $ f (from a) (from b)
        wrapF :: Functor f => (a -> f a) -> b -> f b
        wrapF f a = fmap to $ f (from a)

instance Convertible Cell Cell where
        to = id
        from = id
