{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module World.Data
( module World.Data.Cell
, module World.Data.Slider
, module World.Data
, V2(..)
) where

import World.Data.Cell
import World.Data.Slider
import Data.Maybe (listToMaybe, catMaybes)
import Data.List ((\\))

import Control.Applicative
import FreeGame (V2(..), Vec2)

import Control.Parallel.Strategies
import Control.DeepSeq
import Debug.Trace

cellStatic :: (Num a) => a
cellStatic = 40 

defaultWidth, defaultHeight, frameLoop :: (Num a) => a
defaultWidth = 800
defaultHeight = 600
frameLoop = 1500

transMod = 0.25

type Coord = Vec2


type Board = [Cell]
type Rect = (Double,Double,Double,Double)

newtype SizeTuple = SizeTuple (Int, Int) deriving (Eq, Show)

sizeTupleCell :: SizeTuple -> Cell
sizeTupleCell (SizeTuple t) = Cell t

sizeTupleRow :: SizeTuple -> Int
sizeTupleRow (SizeTuple (r, _)) = r
sizeTupleCol :: SizeTuple -> Int
sizeTupleCol (SizeTuple (_, c)) = c

maxCoord :: SizeTuple -> Coord
maxCoord (SizeTuple (x,y)) = V2 (fromIntegral x) (fromIntegral (y))


type RCoord = (Slider, Slider)

data Direct = UP | DOWN | LEFT | RIGHT deriving (Eq, Show, Ord)

instance (NFData Direct) where
    rnf a = a `seq` ()

turnBack :: Direct -> Direct
turnBack RIGHT = LEFT
turnBack LEFT  = RIGHT
turnBack UP    = DOWN
turnBack DOWN  = UP

cellMoves :: [Direct] -> [Direct] -> Cell -> Cell
cellMoves dirs filter_dir c = let dirs' = dirs \\ filter_dir
                               in foldr adjacentCell c dirs'
  
adjacentDirections :: Board -> Cell -> [Direct]
adjacentDirections board c = catMaybes $ ( map (adjacentDirection c) board `using` parList rdeepseq)

adjacentFieldDirections board c = catMaybes $ ( map (adjacentFieldDirection c) board `using` parList rdeepseq)


_peripheralCells :: ((Int, Int) -> a) -> Cell -> Int -> [a]
_peripheralCells ctr (Cell (r,c)) w = [ctr (r',c') | r' <- [r-w..r+w] ,c' <- [c-w..c+w]]


peripheralCells :: Cell -> Int -> [Cell]
peripheralCells c w = _peripheralCells Cell c w

peripheralFieldCells :: FieldCell -> Int -> [FieldCell]
peripheralFieldCells (FieldObject c) i = _peripheralCells fcell c i

adjacentCells :: Board -> Cell -> [Cell]
adjacentCells board c = filter (`elem` board) (peripheralCells c 1)


cellOuterDirection :: SizeTuple -> FieldCell -> [Direct]
cellOuterDirection st to = v st to ++ h st to 
    where 
         v (SizeTuple (fr, fc)) 
           (FieldObject (Cell (tr, tc))) | fc < tc = [RIGHT]
                                         | tc <= 0 = [LEFT]
                                         | otherwise = []
         h (SizeTuple (fr, fc)) 
           (FieldObject (Cell (tr, tc))) | fr < tr = [DOWN] 
                                         | tr <= 0 = [UP]
                                         | otherwise = []

edgeTuple :: SizeTuple -> Direct -> Cell -> (Int, Int)
edgeTuple (SizeTuple (sr,sc)) LEFT  (Cell (r, c)) = (r , 1)
edgeTuple (SizeTuple (sr,sc)) RIGHT (Cell (r, c)) = (r, sc)
edgeTuple (SizeTuple (sr,sc)) UP    (Cell (r, c)) = (1,  c)
edgeTuple (SizeTuple (sr,sc)) DOWN  (Cell (r, c)) = (sr, c)

data Edge = UpperLeft | LowerLeft | UpperRight | LowerRight
               deriving (Eq,Show)

allEdge = [UpperLeft,LowerLeft,LowerRight,UpperRight]

celledge :: Edge -> Cell -> Cell
celledge UpperLeft  (Cell (r,c)) = Cell (r, c) 
celledge LowerLeft  (Cell (r,c)) = Cell (r, c+1)
celledge LowerRight (Cell (r,c)) = Cell (r+1, c+1) 
celledge UpperRight (Cell (r,c)) = Cell (r+1, c)   


allDirection :: [Direct]
allDirection = [UP, LEFT, DOWN, RIGHT]

fromDirect :: Direct -> Direct -> Edge
fromDirect UP    LEFT  = UpperLeft
fromDirect LEFT  UP    = UpperLeft
fromDirect DOWN  LEFT  = LowerLeft
fromDirect LEFT  DOWN  = LowerLeft
fromDirect RIGHT DOWN  = LowerRight
fromDirect DOWN  RIGHT = LowerRight
fromDirect UP    RIGHT = UpperRight
fromDirect RIGHT UP    = UpperRight

directLineEdge :: Direct -> [Edge]
directLineEdge UP    = [UpperRight, UpperLeft]
directLineEdge LEFT  = [UpperLeft , LowerLeft]
directLineEdge DOWN  = [LowerLeft , LowerRight]
directLineEdge RIGHT = [LowerRight, UpperRight]

adjacentCell :: Direct -> Cell -> Cell
adjacentCell UP    (Cell (r,c)) = Cell (r-1,c)
adjacentCell DOWN  (Cell (r,c)) = Cell (r+1,c)
adjacentCell LEFT  (Cell (r,c)) = Cell (r,c-1)
adjacentCell RIGHT (Cell (r,c)) = Cell (r,c+1)

adjacentCell' :: Direct -> Cell -> Cell
adjacentCell' UP    (Cell (r,c)) = Cell (r-1,c+1)
adjacentCell' DOWN  (Cell (r,c)) = Cell (r+1,c-1)
adjacentCell' LEFT  (Cell (r,c)) = Cell (r-1,c-1)
adjacentCell' RIGHT (Cell (r,c)) = Cell (r+1,c+1)


adjacentDirection :: Cell -> Cell -> Maybe Direct
adjacentDirection ours theirs = listToMaybe $ filter (\d -> adjacentCell d ours == theirs) allDirection 

adjacentFieldDirection :: FieldCell -> FieldCell -> Maybe Direct
adjacentFieldDirection (FieldObject ours) (FieldObject theirs) = adjacentDirection ours theirs

class CellHolder c where
    cellValue :: c -> Cell
    holdering :: Cell -> c

newtype MapObject a = MapObject a deriving (Eq, Ord, Show, Functor)
newtype FieldObject a = FieldObject a deriving (Eq, Ord, Show, Functor)

instance Applicative MapObject where
    pure = MapObject
    (MapObject f) <*> (MapObject a) = MapObject $ f a
instance Applicative FieldObject where
    pure = FieldObject
    (FieldObject f) <*> (FieldObject a) = FieldObject $ f a

type MapCell = MapObject Cell
type FieldCell = FieldObject Cell

instance (NFData FieldCell) where
    rnf a = a `seq` ()

fcell t = FieldObject $ Cell t
mcell t = MapObject $ Cell t
 
instance CellHolder MapCell where
    cellValue (MapObject c) = c
    holdering = MapObject 
instance CellHolder FieldCell where
    cellValue (FieldObject c) = c
    holdering = FieldObject 

slide0 = slider 0

center :: RCoord
center = (slide0,slide0)

slidePositive = slider 100
slideNegative = slider (-100)

_slideUpX = slideUp 16 
_slideDownX = slideDown 16 

slideRCoord :: RCoord -> Direct -> RCoord
slideRCoord (rcx, rcy) RIGHT = (_slideUpX rcx, rcy) 
slideRCoord (rcx, rcy) LEFT  = (_slideDownX rcx, rcy) 
slideRCoord (rcx, rcy) UP    = (rcx, _slideDownX rcy) 
slideRCoord (rcx, rcy) DOWN  = (rcx, _slideUpX rcy) 

normalMapping :: SizeTuple -> MapCell -> FieldCell -> Coord
normalMapping st mc fc = V2 (fromIntegral (c * cellStatic)) (fromIntegral (r * cellStatic))
    where (MapObject msc) = fmap (sizeTupleCell st *) mc
          (Cell (r,c)) = cellValue fc + msc 

rectCell :: Double -> Coord -> SizeTuple -> MapCell -> FieldCell -> [Coord]
rectCell m origin st mc c = map (\e -> cornerPoint origin e st mc c) allEdge `using` parList rdeepseq

getRCoord :: Coord -> RCoord -> Coord
getRCoord crd (rx, ry) = crd + V2 (fromR crd rx) (fromR crd ry)

fromR :: Coord -> Slider -> Double
fromR crd rc = let persent rc = fromIntegral $ rc^.percent^.rangeInsideValue :: Double
                   sliderSize = cellLong transMod crd / 2
                in sliderSize * persent rc / 100

cellLong :: Double -> Coord -> Double 
cellLong m v@(V2 x y) = abs $ x'' - x'
    where 
      v' = V2 (x+cellStatic) y
      x'  = normalXmap v
      x'' = normalXmap v'

normalXmap :: Coord -> Double
normalXmap (V2 x y) = x + transMod * xdiff * ydiff
    where
        ydiff = 1 - y/defaultHeight 
        xdiff = defaultWidth/2 - x

cornerPoint :: Coord -> Edge -> SizeTuple -> MapCell -> FieldCell -> Coord
cornerPoint vp edge st mc c = let np = vp + normalMapping st mc (fmap (celledge edge) c)
                               in normalTrans np
    where
        normalTrans :: Coord -> Coord
        normalTrans v@(V2 x y) = V2 (normalXmap v) y'
            where
                n  = ceiling y `div` cellStatic 
                b  = fromIntegral $ round y `mod` cellStatic 
                cellLong' yy = cellLong transMod (V2 x yy)
                y' = cellLong' y * b / cellStatic
                        + if n <= 0 then -cellStatic
                                    else iterate (\t -> t + cellLong' t) 0 !! n  
                        - cellStatic
                        
cornerPointOrigin :: Coord
cornerPointOrigin = cornerPoint (V2 0 0) UpperLeft (SizeTuple (15,15)) (mcell (1,1)) (fcell (1,1))


fieldSizeTrans :: Coord -> Coord
fieldSizeTrans vp = ncrd - cornerPointOrigin
    where ncrd = cornerPoint vp UpperLeft (SizeTuple (15,15)) (mcell (2,2)) (fcell (1,1))



fieldPosition :: Coord -> SizeTuple -> MapCell -> FieldCell -> RCoord -> Coord
fieldPosition vp st mc c rc = getRCoord (cornerPoint vp UpperLeft st mc c) rc

