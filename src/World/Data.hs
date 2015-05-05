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

import FreeGame (V2(..), Vec2)

cellStatic :: (Num a) => a
cellStatic = 40 

defaultWidth, defaultHeight, frameLoop :: (Num a) => a
defaultWidth = 800
defaultHeight = 600
frameLoop = 1500

transMod = 0.25

-- type Coord = (Int, Int)

type Coord = Vec2

type Board = [Cell]
type Rect = (Double,Double,Double,Double)

newtype SizeTuple = SizeTuple (Int, Int)

sizeTupleCell :: SizeTuple -> Cell
sizeTupleCell (SizeTuple t) = Cell t

maxCoord :: SizeTuple -> Coord
maxCoord (SizeTuple (x,y)) = V2 (fromIntegral x) (fromIntegral y)

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

adjacentCell' :: Cell -> Direct -> Cell
adjacentCell' (Cell (r,c)) UP    = Cell (r-1,c+1)
adjacentCell' (Cell (r,c)) DOWN  = Cell (r+1,c-1)
adjacentCell' (Cell (r,c)) LEFT  = Cell (r-1,c-1)
adjacentCell' (Cell (r,c)) RIGHT = Cell (r+1,c+1)


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

normalMapping :: Int  -> Cell -> Coord
normalMapping long (Cell (r,c)) = V2 (fromIntegral (c * long)) (fromIntegral (r * long))

rectCell :: Double -> SizeTuple -> Coord -> Cell -> [Coord]
rectCell m msize origin c = map (\e -> cornerPoint msize origin e c ) allEdge

getRCoord :: RCoord -> Coord
getRCoord (rx, ry) = V2 (fromR rx) (fromR ry)
    where 
      sliderSize' = fromIntegral.sliderSize 
      persent rc = fromIntegral $ rc^.percent^.rangeInsideValue :: Double
      fromR :: Slider -> Double
      fromR rc = sliderSize' rc * (persent rc / 100) + cellStatic / 2


yTrans = \y h -> (h - y) / h

stripeVXModifier :: Fractional a => a -> a -> a -> a
stripeVXModifier x w m = x + ((w / 2 - x) * m)

cellLong :: Double -> Coord -> Double 
cellLong m (V2 x y) = magni
    where magni = (1.625 - yTrans y defaultWidth) 


normalTrans ::  Double ->  Coord -> Coord
normalTrans m (V2 x y) = V2 (stripeVXModifier x defaultWidth (ydiff * m)) (cellSumLong m y)
    where
        ydiff = yTrans y defaultHeight
        cellSumLong :: Double -> Double -> Double
        cellSumLong m y = let ylist  = [y, y-cellStatic ..]
                              ylist' = takeWhile (> -cellStatic*5) ylist
                           in foldr (\h a -> a + cellStatic * (cellLong m (V2 0 (h)))) (y / cellStatic) ylist'

cornerPoint :: SizeTuple -> Coord -> Edge -> Cell -> Coord
cornerPoint sm vp edge c = normalTrans transMod $ vp + normalMapping cellStatic edgeCell
    where
        edgeCell = celledge c edge


fieldPosition :: SizeTuple -> Coord -> Cell -> RCoord -> Coord
fieldPosition sm o c rc = cornerPoint sm (o + getRCoord rc) UpperLeft c 


