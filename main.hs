{-# LANGUAGE ImplicitParams, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where
import FreeGame
import Control.Monad 
import Control.Lens 
import qualified Data.Map as Map
import qualified Data.Array as Array
import Data.Maybe (catMaybes, fromMaybe, fromJust, listToMaybe)
import Data.IORef
import qualified Data.List as List
import Data.Range.Range 
import Paths_Grids

defaultWidth, defaultHeight :: Double
defaultWidth = 640
defaultHeight = 480

loadBitmapsWith [e|getDataFileName|] "images"

type Coord = (Int,Int)

coordVec :: Coord -> Vec2
coordVec (x, y) =  V2 (fromIntegral x) (fromIntegral y)

data Ranged a = Ranged { range :: Range a
                       , value :: a }

--data RangedCell = RangedCell { rangedCell_range :: Range Cell
--                             , rangedCell_cell :: Cell
--                             }
--
--
--
newtype Size2 = Size2 (Int,Int) deriving (Eq,Show,Ord)

data RangedCell = RangedCell { 
                               size :: Size2
                             , r_cell  :: Cell
                             } deriving(Eq,Show,Ord)


maxbound rc = last $ fromRanges [rc]

instance Enum(RangedCell) where
    succ rc | c == mc =  RangedCell siz (cel + Cell(1,0))
                                | otherwise = RangedCell siz (cel + Cell(0,1))
      where siz@(Size2 (mr, mc)) = size rc
            cel@(Cell(r, c)) = r_cell rc

data Slider = Slider { inner_range :: Range Int
                     , slider_size :: Int
                     , percent :: Ranged Double
                     }

slider max per = Slider {inner_range = SpanRange (-max) (max)
                        ,slider_size = max
                        ,percent = ranged (SpanRange (-100) 100) per}
                            

ranged :: Ord a => Range a -> a -> Ranged a
ranged r v = if inRange r v then Ranged {range = r, value = v} else undefined

type RCoord = (Slider, Slider)

getRCoord :: RCoord -> Vec2
getRCoord (rx, ry) = let x = (((fromIntegral.slider_size) rx) * ((value.percent) rx)) / 100 
                         y = (((fromIntegral.slider_size) ry) * ((value.percent) ry)) / 100
                         in V2 x y

cell_long :: Num a => a
cell_long = 40 

newtype Cell = Cell (Int, Int) deriving (Eq,Show,Ord)
instance Num(Cell) where 
    Cell (a,b) + Cell (c,d) = Cell (a+c, b+d)
    Cell (a,b) * Cell (c,d) = Cell (a*c, b*d)
    Cell (a,b) - Cell (c,d) = Cell (a-c, b-d)
    abs (Cell (a,b)) = Cell (abs a, abs b)
    signum (Cell (a,b)) = Cell (signum a, signum b) 
    fromInteger i = Cell (fromInteger i, fromInteger i)

type Board = [Cell]
type Rect = (Double,Double,Double,Double)

data Direct = UP | DOWN | LEFT | RIGHT deriving (Eq, Show)

data FState = FNormal | FBlock 
            deriving (Eq,Ord)

data CharaEnty = CharaEnty {
                   _hp :: Int ,
                   _direct :: Direct ,
                   _cellObj :: CellObj
}


data CellObj = CellObj {
                  _cell :: Cell ,
                  _pos :: RCoord,
                  _movable :: Bool ,
                  _block :: Bool 
}

type CellRange  = (Cell, Cell)

data FieldMap = CurrentMap {
                  _mpos :: Cell ,
                  _msizie :: CellRange,
                  _mobj :: [CellObj] ,
                  _mchr :: [CharaEnty],
                  _backpict :: Map.Map CellRange Bitmap
}

type Field = Array.Array Cell [FState]

filedMap' = Map.fromList [(Cell (5,5),FBlock)]

makeLenses ''CharaEnty
makeLenses ''CellObj
makeLenses ''FieldMap

type BackPicture = Map.Map (Range Cell) Bitmap


fieldMap :: FieldMap
fieldMap =  CurrentMap (Cell (5,5)) (Cell(0,0),Cell(25,25)) [CellObj (Cell(1,1)) (slider cell_long 0,slider cell_long 0) False True
                                                            ,CellObj (Cell(2,2))  (slider cell_long 0,slider cell_long 0) False True
                                                            ] [] (Map.fromList [((Cell(0,0),Cell(10,10)), _maptips_ami_png)])

allDirection :: [Direct]
allDirection = [UP, LEFT, DOWN, RIGHT]

origin :: Vec2
origin = V2 0 0 

main :: IO (Maybe a)
main = runGame Windowed (Box (V2 0 0) (V2 defaultWidth defaultHeight)) $ do
    font <- embedIO $ loadFont "VL-Gothic-Regular.ttf"
    let ?font = font
    let  scell = CharaEnty 100 RIGHT $ CellObj (Cell(3,3)) (slider cell_long 0,slider cell_long 0) False True
    mainLoop scell

mainLoop :: (?font :: Font) => CharaEnty -> Game a
mainLoop charaEnty@(CharaEnty hp mdir (CellObj c pos mb bl )) = do
    let origin = V2 0 0
        thick = 2
        whole_rect = (0,0,defaultWidth, defaultHeight)

    io_me    <- embedIO $ newIORef charaEnty
    io_field <- embedIO $ newIORef fieldMap 

    foreverFrame $ do 
        color blue
            $ fillCell (V2 0 0) cell_long $ Cell (5,5)
        me <- embedIO $ readIORef io_me
        let c' = me ^. cellObj ^. cell

        (current@(CurrentMap _ msize os cs bp)) <- embedIO $ readIORef io_field

        translate (V2 50 50) $ bitmap _front0_png
        mapM_ id [
            -- forM_ (Map.keys (fieldMap ^. backpict)) 
            --       (\cr -> forM_ (fromJust.Map.lookup cr) bitmap)
            color green
                $ translate (V2 40 400)
                $ text ?font cell_long $ show hp
            ,
            color red 
                $ ownCell origin whole_rect cell_long io_me io_field
            ,
            color yellow 
                $ thickness 3 
                $ renderGrids whole_rect cell_long
            ,
            color black 
                $ translate (V2 150 150) 
                $ text ?font 30 $ show c'
            ,
            color black 
                $ translate (V2 40 40) 
                $ text ?font 40 "Free World"
            ]

key_map :: Map.Map Key Direct
key_map = Map.fromList _tbl
    where _tbl = [(KeyJ, DOWN)
                 ,(KeyK, UP)
                 ,(KeyH, LEFT)
                 ,(KeyL, RIGHT)]

turnBack :: Direct -> Direct
turnBack RIGHT = LEFT
turnBack LEFT  = RIGHT
turnBack UP    = DOWN
turnBack DOWN  = UP

ownCell :: (?font :: Font) => Vec2 -> Rect -> Double -> IORef CharaEnty -> IORef FieldMap -> Frame ()
ownCell origin whole_rect cell_long io_me io_field = do
    me <- embedIO $ readIORef io_me
    let mhp = me ^. hp
        mdir = me ^. direct
        mobj = me ^. cellObj
    (CurrentMap mcell msize fobj_ary fchars bp) <- embedIO $ readIORef io_field

    current_inp <- filterM keyDown $ Map.keys key_map 
    let inp_directs :: [Direct]
        inp_directs = map (\k -> fromJust $ Map.lookup k key_map) current_inp
        obj_cells = map (^. cell) fobj_ary
        blocked_dir = adjacentDirections obj_cells (mobj^.cell)
        ncell = sum_move (mobj^.cell) inp_directs blocked_dir
        mdir' = fromJust $ if ncell /= (mobj^.cell) then adjacentDirection (mobj^.cell) ncell else Just mdir

    ainp <- keyPress KeyA
    when ainp $ fillCells origin cell_long $ peripheralCells (mobj^.cell) 1

    embedIO $ writeIORef io_me $ CharaEnty mhp mdir' $ CellObj ncell (mobj^.pos) (mobj^.block) (mobj^.movable) 
    color yellow $ fillCells origin cell_long obj_cells
    translate  (picPos origin cell_long (mobj^.cell) (mobj^.pos))
        $ bitmap _front0_png
    fillCell origin cell_long ncell
    color blue $ tCell origin cell_long ncell mdir' $ circle 5

sum_move :: Cell -> [Direct] -> [Direct] -> Cell
sum_move c dirs filter_dir = let dirs' = dirs List.\\ filter_dir
                             in flip (foldr (flip adjacentCell)) dirs' c

renderGrids :: Rect -> Double -> Frame ()
renderGrids (x0, y0, w, h) interval = renderStripeH (x0,y0,w,h) interval 
                                      >> renderStripeV (x0,y0,w,h) interval

renderStripeV :: Rect -> Double -> Frame ()
renderStripeV (x0, y0, w, h) interval  = zipWithM_ (\x y -> line [x,y])
            [V2 (x0 + p)  y0      | p <- [0,interval .. w]] 
            [V2 (x0 + p) (y0 + h) | p <- [0,interval ..]] 

renderStripeH :: Rect -> Double -> Frame ()
renderStripeH (x0, y0, w, h) interval = zipWithM_ (\x y -> line [x,y])
            [V2 x0       (y0 + p) | p <- [0,interval .. h]] 
            [V2 (x0 + w) (y0 + p) | p <- [0,interval ..]] 

renderStripeV' (x0, y0, w, h) interval = zipWithM_ (\x y -> line [x,y])
            [V2 (diff (x0 + p))  y0 | p <- [0,interval .. w]] 
            [V2 (x0 + p) (y0 + h) | p <- [0,interval ..]] 
        where
            diff x = let c = x0 + w / 2
                         p = c - x
                     in  x + p * 0.3 

data Edge = UpperLeft | LowerLeft | UpperRight | LowerRight
               deriving (Eq,Show)

allEdge = [UpperLeft,LowerLeft,LowerRight,UpperRight]

fromDirect :: Direct -> Direct -> Edge
fromDirect UP    LEFT  = UpperLeft
fromDirect LEFT  UP    = UpperLeft
fromDirect DOWN  LEFT  = LowerLeft
fromDirect LEFT  DOWN  = LowerLeft
fromDirect RIGHT DOWN  = LowerRight
fromDirect DOWN  RIGHT = LowerRight
fromDirect UP    RIGHT = UpperRight
fromDirect RIGHT UP    = UpperRight

directLine :: Direct -> [Edge]
directLine UP    = [UpperRight, UpperLeft]
directLine LEFT  = [UpperLeft , LowerLeft]
directLine DOWN  = [LowerLeft , LowerRight]
directLine RIGHT = [LowerRight, UpperRight]

adjacentCell :: Cell -> Direct -> Cell
adjacentCell (Cell (r,c)) UP    = Cell (r-1,c)
adjacentCell (Cell (r,c)) DOWN  = Cell (r+1,c)
adjacentCell (Cell (r,c)) LEFT  = Cell (r,c-1)
adjacentCell (Cell (r,c)) RIGHT = Cell (r,c+1)

celledge :: Cell -> Edge -> Cell
celledge (Cell (r,c)) UpperLeft  = Cell (r, c) 
celledge (Cell (r,c)) LowerLeft  = Cell (r, c+1)
celledge (Cell (r,c)) LowerRight = Cell (r+1, c+1) 
celledge (Cell (r,c)) UpperRight = Cell (r+1, c)   

adjacentDirection :: Cell -> Cell -> Maybe Direct
adjacentDirection ours theirs = listToMaybe $ filter (\d -> adjacentCell ours d == theirs) allDirection 

rectCell :: Vec2 -> Double -> Cell -> [Vec2]
rectCell origin long c = map (\e -> cornerPoint origin long e c) allEdge

edgeIn :: Affine p => Vec2 -> Double -> Cell -> p a -> p a
edgeIn o l c = translate $ cornerPoint o l UpperLeft c

renderCellOutline :: Picture2D p => Vec2 -> Double -> Direct -> Cell -> p ()
renderCellOutline origin long dir c = line $ map (\edge -> cornerPoint origin long edge c)  $ directLine dir

cornerPoint :: Vec2 -> Double -> Edge -> Cell -> Vec2
cornerPoint origin long edge c = let cpoint :: Cell -> Vec2
                                     cpoint (Cell (r,c)) = origin + V2 (fromIntegral c * long) (fromIntegral r * long) 
                                     in cpoint $ celledge c edge

picPos :: Vec2 -> Double -> Cell -> RCoord -> Vec2
picPos o l c rc = cornerPoint o l UpperLeft c + o + ((getRCoord) rc)

-- transPic :: Picture2D p => Vec2 -> p a -> Cell -> RCoord -> RCoord ->  p ()
-- transPic origin p c rx ry = translate $ picPos origin c rx ry >> p

fillCell :: Picture2D p => Vec2 -> Double -> Cell -> p () 
fillCell origin long c = polygon $ rectCell origin long c 

tCell :: Affine p => Vec2 -> Double -> Cell -> Direct -> p a -> p a
tCell origin long c dir = translate $ (/4) . sum $ rectCell origin long $ adjacentCell c dir

strokeCell :: Picture2D p => Vec2 -> Double -> Cell -> p ()
strokeCell origin long c = polygonOutline $ rectCell origin long c 

fillCells :: (Monad p, Picture2D p) => Vec2 -> Double -> [Cell] -> p ()
fillCells origin long cs = mapM_ (fillCell origin long) cs 

strokeCells :: (Monad p, Picture2D p) => Vec2 -> Double -> [Cell] -> p ()
strokeCells origin long cs =
    let  renderCellOutline' = renderCellOutline origin long
         render c = let  adjacents = adjacentDirections cs c
                    in mapM_ (\d -> renderCellOutline' d c) adjacents
     in mapM_ render cs

adjacentDirections :: Board -> Cell -> [Direct]
adjacentDirections board c = catMaybes $ map (adjacentDirection c) board

peripheralCells :: Cell -> Int -> [Cell]
peripheralCells (Cell (r,c)) w = [Cell (r',c') | r' <- [r-w..r+w] ,c' <- [c-w..c+w]]

adjacentCells :: Board -> Cell -> [Cell]
adjacentCells board c = filter (`elem` board) (peripheralCells c 1)
