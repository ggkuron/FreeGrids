module World.Data
( module Data.Cell
, module Data.Slider
, module World.Data
) where

import Data.Cell
import Data.Slider
import Data.Maybe (listToMaybe, catMaybes)
import Data.List ((\\))

cellStatic :: (Num a) => a
cellStatic = 40 

defaultWidth, defaultHeight, frameLoop :: (Num a) => a
defaultWidth = 800
defaultHeight = 600
frameLoop = 1500

transMod = 0.25



type XCoord = Int -- →
type YCoord = Int -- ↓
type Coord = (XCoord, YCoord)
type Board = [Cell]
type Rect = (Double,Double,Double,Double)

newtype SizeTuple = SizeTuple Coord

sizeTupleCell :: SizeTuple -> Cell
sizeTupleCell (SizeTuple c) = Cell c

maxCoord :: SizeTuple -> Coord
maxCoord (SizeTuple crd) = crd

type RCoord = (Slider, Slider)

data Direct = UP | DOWN | LEFT | RIGHT deriving (Eq, Show, Ord)

turnBack :: Direct -> Direct
turnBack RIGHT = LEFT
turnBack LEFT  = RIGHT
turnBack UP    = DOWN
turnBack DOWN  = UP

cellMoves :: Cell -> [Direct] -> [Direct] -> Cell
cellMoves c dirs filter_dir = let dirs' = dirs \\ filter_dir
                             in flip (foldr (flip adjacentCell)) dirs' c

adjacentDirections :: Board -> Cell -> [Direct]
adjacentDirections board c = catMaybes $ map (adjacentDirection c) board

peripheralCells :: Cell -> Int -> [Cell]
peripheralCells (Cell (r,c)) w = [Cell (r',c') | r' <- [r-w..r+w] ,c' <- [c-w..c+w]]

adjacentCells :: Board -> Cell -> [Cell]
adjacentCells board c = filter (`elem` board) (peripheralCells c 1)


data Edge = UpperLeft | LowerLeft | UpperRight | LowerRight
               deriving (Eq,Show)

allEdge = [UpperLeft,LowerLeft,LowerRight,UpperRight]

celledge :: Cell -> Edge -> Cell
celledge (Cell (r,c)) UpperLeft  = Cell (r, c) 
celledge (Cell (r,c)) LowerLeft  = Cell (r, c+1)
celledge (Cell (r,c)) LowerRight = Cell (r+1, c+1) 
celledge (Cell (r,c)) UpperRight = Cell (r+1, c)   

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

adjacentCell :: Cell -> Direct -> Cell
adjacentCell (Cell (r,c)) UP    = Cell (r-1,c)
adjacentCell (Cell (r,c)) DOWN  = Cell (r+1,c)
adjacentCell (Cell (r,c)) LEFT  = Cell (r,c-1)
adjacentCell (Cell (r,c)) RIGHT = Cell (r,c+1)

adjacentDirection :: Cell -> Cell -> Maybe Direct
adjacentDirection ours theirs = listToMaybe $ filter (\d -> adjacentCell ours d == theirs) allDirection 


slide0 = slider (cellStatic`div`2) 0

center :: RCoord
center = (slide0,slide0)

slidePositive = slider (cellStatic`div`2)  100
slideNegative = slider (cellStatic`div`2) (-100)

_slideUpX = slideUp 16 
_slideDownX = slideDown 16 

slideRCoord :: RCoord -> Direct -> RCoord
slideRCoord (rcx, rcy) RIGHT = (_slideUpX rcx, rcy) 
slideRCoord (rcx, rcy) LEFT  = (_slideDownX rcx, rcy) 
slideRCoord (rcx, rcy) UP    = (rcx, _slideDownX rcy) 
slideRCoord (rcx, rcy) DOWN  = (rcx, _slideUpX rcy) 

nextDirect :: RCoord -> (RCoord, [Direct])
nextDirect (rcx, rcy) = 
    let slideUpdate s up down | isSlideUpped s = (slideNegative, [up])
                              | isSlideDowned s = (slidePositive, [down])
                              | otherwise = (s, [])
        (rcx', xdir) = slideUpdate rcx RIGHT LEFT
        (rcy', ydir) = slideUpdate rcy DOWN UP
    in ((rcx', rcy'), xdir ++ ydir)
                  


