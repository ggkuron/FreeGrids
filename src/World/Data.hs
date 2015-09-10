{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module World.Data
( module World.Data.Cell
, module World.Data.Slider
, module World.Data
, V2(..)
) where

import Prelude hiding (sum)
import World.Data.Cell
import World.Data.Slider
import Data.Maybe (listToMaybe, catMaybes)
import Data.List ((\\))
import qualified Data.Foldable as F

-- import Control.Applicative
import FreeGame (Vec2)
import Linear.V2

import Control.Parallel.Strategies
import Control.DeepSeq

cellStatic :: (Num a) => a
cellStatic = 40 

defaultWidth, defaultHeight, frameLoop :: (Num a) => a
defaultWidth = 800
defaultHeight = 600
frameLoop = 1500

transMod :: Double
transMod = 0.25

type Coord = Vec2

type Rect = (Double,Double,Double,Double)

newtype SizeTuple = SizeTuple (Int :!: Int) deriving (Eq, Ord, Show)

type RCoord = (Slider :!: Slider)

data Direct = UP | DOWN | LEFT | RIGHT deriving (Eq, Show, Ord)

instance NFData Direct where
    rnf a = a `seq` ()

turnBack :: Direct -> Direct
turnBack RIGHT = LEFT
turnBack LEFT  = RIGHT
turnBack UP    = DOWN
turnBack DOWN  = UP

cellMoves :: [Direct] -> [Direct] -> Cell -> Cell
cellMoves dirs filter_dir c = let dirs' = dirs \\ filter_dir
                               in foldr adjacentCell c dirs'
  
adjacentDirections :: [Cell] -> Cell -> [Direct]
adjacentDirections board c = catMaybes $ ( map (adjacentDirection c) board `using` parList rdeepseq)

aroundCells :: Int -> Cell -> [Cell]
aroundCells w (Cell (r :!: c)) = [Cell (r' :!: c') | r' <- [r-w..r+w] ,c' <- [c-w..c+w]]

aroundCells2 :: Cell -> Cell -> [Cell]
aroundCells2 (Cell (r :!: c)) (Cell (rw :!: cw)) = [Cell (r' :!: c') | r' <- [r-rw..r+rw] ,c' <- [c-cw..c+cw]]

adjacentCells :: [Cell] -> Cell -> [Cell]
adjacentCells board c = filter (`elem` board) (aroundCells 1 c)


edgeTuple :: SizeTuple -> Direct -> Cell -> (Int :!: Int)
edgeTuple _ LEFT  (Cell (r :!: _)) = (r :!: 1)
edgeTuple (SizeTuple (_ :!: sc)) RIGHT (Cell (r :!: _)) = (r :!: sc)
edgeTuple _ UP    (Cell (_ :!: c)) = (1 :!: c)
edgeTuple (SizeTuple (sr :!: _)) DOWN  (Cell (_ :!: c)) = (sr :!: c)

data Edge = UpperLeft | LowerLeft | UpperRight | LowerRight
               deriving (Eq,Show)

allEdge :: [Edge]
allEdge = [UpperLeft,LowerLeft,LowerRight,UpperRight]

celledge :: Edge -> Cell -> Cell
celledge UpperLeft  (Cell (r :!: c)) = Cell (r :!: c) 
celledge LowerLeft  (Cell (r :!: c)) = Cell (r+1 :!: c)
celledge LowerRight (Cell (r :!: c)) = Cell (r+1 :!: c+1) 
celledge UpperRight (Cell (r :!: c)) = Cell (r :!: c+1)   


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
fromDirect _ _ = undefined

directLineEdge :: Direct -> [Edge]
directLineEdge UP    = [UpperRight, UpperLeft]
directLineEdge LEFT  = [UpperLeft , LowerLeft]
directLineEdge DOWN  = [LowerLeft , LowerRight]
directLineEdge RIGHT = [LowerRight, UpperRight]

adjacentCell :: Direct -> Cell -> Cell
adjacentCell UP    (Cell (r :!: c)) = Cell (r-1 :!: c)
adjacentCell DOWN  (Cell (r :!: c)) = Cell (r+1 :!: c)
adjacentCell LEFT  (Cell (r :!: c)) = Cell (r :!: c-1)
adjacentCell RIGHT (Cell (r :!: c)) = Cell (r :!: c+1)


adjacentCell' :: Direct -> Cell -> Cell
adjacentCell' UP    (Cell (r :!: c)) = Cell (r-1 :!: c+1)
adjacentCell' DOWN  (Cell (r :!: c)) = Cell (r+1 :!: c-1)
adjacentCell' LEFT  (Cell (r :!: c)) = Cell (r-1 :!: c-1)
adjacentCell' RIGHT (Cell (r :!: c)) = Cell (r+1 :!: c+1)

adjacentDirection :: Cell -> Cell -> Maybe Direct
adjacentDirection ours theirs = listToMaybe $ filter (\d -> adjacentCell d ours == theirs) allDirection 

newtype ACell = ACell Cell deriving (Eq, Show, Ord)
newtype BCell = BCell Cell deriving (Eq, Show, Ord)
newtype WorldCell = WorldCell Cell deriving (Eq, Show)

instance Ord WorldCell where
        l`compare` r = (fromEnum l) `compare` (fromEnum r) 

class (Enum a, Convertible Cell a) => WorldIndex a where
    fieldCell :: a -> WorldCell
    toWorldIndex :: Cell -> a
    toWorldIndex (Cell (r :!: c)) = toEnum $ (15*250)*(r-1) + c - 1

instance Convertible Cell ACell where
    from (ACell c) = c
    to = ACell
instance Convertible Cell BCell where
    from (BCell c) = c
    to = BCell
instance Convertible Cell WorldCell where
    from (WorldCell c) = c
    to = WorldCell
instance Convertible Cell MapCell where
    from = from . fieldCell
    to = toWorldIndex

instance WorldIndex MapCell where
    fieldCell (MapCell 
                (BCell(Cell(mr:!:mc)))
                (ACell(Cell(fr:!:fc)))
              ) = worldCell (15*(mr-1)+fr :!: 15*(mc-1)+fc) 

instance WorldIndex WorldCell where
    fieldCell = id

aCell :: Pair Int Int -> ACell
aCell = ACell . Cell
bCell :: Pair Int Int -> BCell
bCell = BCell . Cell
worldCell :: Pair Int Int -> WorldCell
worldCell = WorldCell . Cell

data MapCell = MapCell BCell ACell
    deriving (Show, Eq, Ord)

instance Bounded ACell where
    minBound = ACell $ Cell(1:!:1)
    maxBound = ACell $ Cell(15 :!: 15)
instance Bounded BCell where
    minBound = BCell $ Cell(1:!:1)
    maxBound = BCell $ Cell(250 :!: 250)

instance Enum ACell where
    succ sc = toEnum $ fromEnum sc + 1
    fromEnum (ACell(Cell(r:!:c))) = (r-1) * 15 + c - 1
    toEnum i = let (arow,acol) = i `divMod` 15
                in aCell (arow+1:!:acol) 

instance Enum WorldCell where
    succ sc = toEnum $ fromEnum sc + 1
    fromEnum (WorldCell(Cell(r:!:c))) = (r-1) * 250 + c - 1
    toEnum i = let (arow,acol) = i `divMod` (250*15)
                in worldCell (arow+1:!:acol) 

instance Enum (BCell) where
    succ sc = toEnum $ fromEnum sc + 1
    fromEnum (BCell(Cell(r:!:c))) = (r-1) * 250 + c - 1
    toEnum i = let (arow,acol) = i `divMod` 250
                in bCell (arow+1 :!: acol) 

instance Enum MapCell where
    succ sc = toEnum $ fromEnum sc + 1
    fromEnum (MapCell (BCell(Cell(br:!:bc))) (ACell(Cell(ar:!:ac))))
     = ((br-1)*15+(ar-1))*(15*250) + ((bc-1)*15+ac-1)
    toEnum i = let (row, col) = i `divMod` (15*250)
                   (br, ar) = row `divMod` 15
                   (bc, ac) = col `divMod` 15
                in mapCell (br+1:!:bc+1) (ar+1:!:ac+1)

mapCell :: Pair Int Int -> Pair Int Int -> MapCell
mapCell m f = MapCell (bCell m) (aCell f)

instance NFData BCell where
    rnf (BCell c) = rnf c `seq` ()
instance NFData ACell where
    rnf (ACell c) = rnf c `seq` ()
instance NFData WorldCell where
    rnf (WorldCell c) = rnf c `seq` ()
instance NFData MapCell where
    rnf (MapCell a b) = rnf a `seq` rnf b `seq` ()


slide0 :: Slider
slide0 = slider 0

center :: RCoord
center = (slide0 :!: slide0)

slidePositive, slideNegative :: Slider
slidePositive = slider 100
slideNegative = slider (-100)

_slideUpX, _slideDownX :: Slider -> Slider
_slideUpX = slideUp (+ 16)
_slideDownX = slideDown $ flip (-) 16

slideRCoord :: RCoord -> Direct -> RCoord
slideRCoord (rcx :!: rcy) RIGHT = (_slideUpX rcx :!: rcy) 
slideRCoord (rcx :!: rcy) LEFT  = (_slideDownX rcx :!: rcy) 
slideRCoord (rcx :!: rcy) UP    = (rcx :!: _slideDownX rcy) 
slideRCoord (rcx :!: rcy) DOWN  = (rcx :!: _slideUpX rcy) 

normalMapping :: WorldCell -> Coord
normalMapping mc =
    let (Cell (r :!: c)) = (from mc) * (Cell (cellStatic:!:cellStatic))
     in V2 (fromIntegral c) (fromIntegral r)

rectCell :: Coord -> WorldCell -> [Coord]
rectCell origin c = map (\e -> cornerPoint origin e c) allEdge 

cellLong :: Coord -> Double 
cellLong (V2 _ y) = cellLong' y

cellLong' :: Double -> Double
cellLong' y = y / 60 + 30

vanishingPoint = V2 (400) (-2400)

normalXmap :: Coord -> Coord
normalXmap v = v&_x+~(transMod * (vanishingPoint^._x - v^._x) * (1 - v^._y/defaultHeight))

normalYmap :: Coord -> Coord
normalYmap v = v&_y*~(23/30)

addRCoord :: Coord -> RCoord -> Coord
addRCoord crd (rx :!: ry) = crd + V2 (fromR rx) (fromR ry)
    where 
      fromR :: Slider -> Double
      fromR rc = let persent = fromIntegral.bval 
                     sliderSize = cellLong crd / 2
                  in sliderSize * persent rc / 100

normalTrans :: Coord -> Coord
normalTrans = normalXmap.normalYmap 

cornerPoint :: Coord -> Edge -> WorldCell -> Coord
cornerPoint vp edge c = normalTrans $ vp + (normalMapping $ wrap (celledge edge) c)

cornerPoint' :: Coord -> WorldCell -> Coord
cornerPoint' vp c = normalTrans $ vp + (normalMapping c)
                       
cornerPointOrigin :: Coord
cornerPointOrigin = cornerPoint' (V2 0 0) $ (worldCell(1:!:1))

fieldSizeTrans :: Coord -> Coord
fieldSizeTrans vp = ncrd - cornerPointOrigin
    where ncrd = cornerPoint' vp $ fieldCell $ mapCell (2 :!: 2) (1 :!: 1)


