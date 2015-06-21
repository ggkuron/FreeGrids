{-# LANGUAGE TypeOperators #-}

module World.Data.Cell
( module World.Data.Cell
) where

newtype Cell = Cell (Int, Int) deriving (Eq,Show,Ord)

type CellLong = Double

cellRow, cellCol :: Cell -> Int
cellRow (Cell (r, _)) = r
cellCol (Cell (_, c)) = c

instance Num(Cell) where 
    Cell (a,b) + Cell (c,d) = Cell (a+c, b+d)
    Cell (a,b) * Cell (c,d) = Cell (a*c, b*d)
    Cell (a,b) - Cell (c,d) = Cell (a-c, b-d)
    abs (Cell (a,b)) = Cell (abs a, abs b)
    signum (Cell (a,b)) = Cell (signum a, signum b) 
    fromInteger i = Cell (fromInteger i, fromInteger i)

