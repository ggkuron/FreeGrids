{-# LANGUAGE ImplicitParams, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where
import FreeGame
import Control.Monad 
import Control.Lens 
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, fromJust, listToMaybe)
import Data.IORef
import qualified Data.List as List

defaultWidth, defaultHeight :: Double
defaultWidth = 640
defaultHeight = 480

newtype Pair a b = Pair (a, b) deriving (Eq, Show)
instance (Num a, Num b) => Num(Pair a b) where 
    Pair (a,b) + Pair (c,d) = Pair (a+c, b+d)
    Pair (a,b) * Pair (c,d) = Pair (a*c, b*d)
    Pair (a,b) - Pair (c,d) = Pair (a-c, b-d)
    abs (Pair (a,b)) = Pair (abs a, abs b)
    signum (Pair (a,b)) = Pair (signum a, signum b) 
    fromInteger i = Pair (fromInteger i, fromInteger i)

type Cell = Pair Int Int
type Board = [Cell]
type Point = Pair Double Double
type Rect = (Double,Double,Double,Double)

data CellBox = CellBox Cell Cell
data Direct = UP | DOWN | LEFT | RIGHT deriving (Eq, Show)

data CharaEnty = CharaEnty {
                   _hp :: Int
                 , _dir :: Direct
                 , _cellObj :: CellObj
}

class TimeObj a where
    nextState :: Double -> a 


-- transTime :: Double
type frame = [(Double, m ())]

data CellObj = CellObj {
                  _pos :: Cell
                , _movable :: Bool
                , _block :: Bool
                , _transTime :: Double
                -- , _state :: Int
}


data FieldMap = CurrentMap {
                  _mpos :: Cell
                , _mobj :: [CellObj]
                , _mchr :: [CharaEnty]
}

makeLenses ''CharaEnty
makeLenses ''CellObj
makeLenses ''FieldMap

fieldMap = CurrentMap (Pair(5,5)) [CellObj (Pair(1,1)) False True 60
                                  ,CellObj (Pair(2,2)) False True 60
                                  ] []

allDirection :: [Direct]
allDirection = [UP, LEFT, DOWN, RIGHT]

main :: IO (Maybe a)
main = runGame Windowed (BoundingBox 0 0 defaultWidth defaultHeight) $ do
    font <- embedIO $ loadFont "VL-Gothic-Regular.ttf"
    let ?font = font
    let  scell = CharaEnty 100 RIGHT $ CellObj (Pair (3,3)) False True 60
    mainLoop scell

mainLoop :: (?font :: Font) => CharaEnty -> Game a
mainLoop charaEnty@(CharaEnty hp mdir (CellObj cell mb bl _)) = do
    let origin = V2 0 0
        cell_long = 40
        thick = 2
        whole_rect = (0,0,defaultWidth, defaultHeight)

    io_me    <- embedIO $ newIORef charaEnty
    io_field <- embedIO $ newIORef fieldMap

    foreverFrame $ do 
        color blue
            $ fillCell (V2 0 0) 40 $ Pair (5,5)
        me <- embedIO $ readIORef io_me
        let cell' = me ^. cellObj ^. pos

        (current@(CurrentMap _ os cs)) <- embedIO $ readIORef io_field
        embedIO $ writeIORef io_field $ current&mobj.each.transTime -~ 1

        mapM id [
            color green
                $ translate (V2 40 400)
                $ text ?font 40 $ show hp
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
                $ text ?font 30 $ show cell'
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
    (CharaEnty hp mdir (CellObj cell mb bl ct)) <- embedIO $ readIORef io_me
    (CurrentMap mcell fobj_ary fchars) <- embedIO $ readIORef io_field

    current_inp <- filterM keyPress $ Map.keys key_map 
    let inp_directs :: [Direct]
        inp_directs = map (\k -> fromJust $ Map.lookup k key_map) current_inp
        obj_cells = map (^. pos) fobj_ary
        blocked_dir = adjacentDirections obj_cells cell
        ncell = sum_move cell inp_directs blocked_dir
        mdir' = fromJust $ if ncell /= cell then adjacentDirection cell ncell else Just mdir

    ainp <- keyPress KeyA
    when ainp $ fillCells origin cell_long $ peripheralCells cell 1

    embedIO $ writeIORef io_me $ CharaEnty hp mdir' $ CellObj ncell mb bl 0
    color yellow $ fillCells origin cell_long obj_cells
    fillCell origin cell_long ncell
    color blue $ tCell origin cell_long ncell mdir' $ circle 5


    -- embedIO $ modifyIORef io_field $ \f -> 


-- timeNew (CellObj p m b tt) t = CellObj p m b t

-- timePasses :: IORef FieldMap -> Double -> IORef FieldMap
-- timePasses ic t = do
                                                   -- &mchr.each.cellObj.each.transTime -~ t 

-- thawJoin :: [a] -> [b] -> (b -> a) -> [a]
-- thawJoin a b thaw =  a ++ $ map thaw b

sum_move :: Cell -> [Direct] -> [Direct] -> Cell
sum_move c dirs filter_dir = let dirs' = dirs List.\\ filter_dir
                             in flip (foldr (flip adjacentCell)) dirs' c

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
adjacentCell (Pair (r,c)) UP    = Pair (r-1,c)
adjacentCell (Pair (r,c)) DOWN  = Pair (r+1,c)
adjacentCell (Pair (r,c)) LEFT  = Pair (r,c-1)
adjacentCell (Pair (r,c)) RIGHT = Pair (r,c+1)

celledge :: Cell -> Edge -> Cell
celledge (Pair (r,c)) UpperLeft  = Pair (r, c) 
celledge (Pair (r,c)) LowerLeft  = Pair (r, c+1)
celledge (Pair (r,c)) LowerRight = Pair (r+1, c+1) 
celledge (Pair (r,c)) UpperRight = Pair (r+1, c)   

adjacentDirection :: Cell -> Cell -> Maybe Direct
adjacentDirection ours theirs = listToMaybe $ filter (\d -> adjacentCell ours d == theirs) allDirection 

rectCell :: Vec2 -> Double -> Cell -> [Vec2]
rectCell origin long cell = map (\e -> cornerPoint origin long e cell) allEdge

edgeIn o l cell = translate $ cornerPoint o l UpperLeft cell

renderCellOutline :: Picture2D p => Vec2 -> Double -> Direct -> Cell -> p ()
renderCellOutline origin long dir cell = line $ map (\edge -> cornerPoint origin long edge cell)  $ directLine dir

cornerPoint :: Vec2 -> Double -> Edge -> Cell -> Vec2
cornerPoint origin long edge cell = let cpoint :: Cell -> Vec2
                                        cpoint (Pair (r,c)) = origin + V2 (fromIntegral c * long) (fromIntegral r * long) 
                                     in cpoint $ celledge cell edge

fillCell :: Picture2D p => Vec2 -> Double -> Cell -> p () 
fillCell origin long cell = polygon $ rectCell origin long cell 

tCell :: Affine p => Vec2 -> Double -> Cell -> Direct -> p a -> p a
tCell origin long cell dir = translate $ (/4) . sum $ rectCell origin long $ adjacentCell cell dir

strokeCell :: Picture2D p => Vec2 -> Double -> Cell -> p ()
strokeCell origin long cell = polygonOutline $ rectCell origin long cell 

fillCells :: (Monad p, Picture2D p) => Vec2 -> Double -> [Cell] -> p ()
fillCells origin long cells = mapM_ (fillCell origin long) cells 

strokeCells :: (Monad p, Picture2D p) => Vec2 -> Double -> [Cell] -> p ()
strokeCells origin long cells =
    let  renderCellOutline' = renderCellOutline origin long
         render c = let  adjacents = adjacentDirections cells c
                    in mapM_ (\d -> renderCellOutline' d c) adjacents
     in mapM_ render cells

adjacentDirections :: Board -> Cell -> [Direct]
adjacentDirections board cell = catMaybes $ map (adjacentDirection cell) board

peripheralCells :: Cell -> Int -> [Cell]
peripheralCells (Pair (r,c)) w = [Pair (r',c') | r' <- [r-w..r+w] ,c' <- [c-w..c+w]]

adjacentCells :: Board -> Cell -> [Cell]
adjacentCells board cell = filter (`elem` board) (peripheralCells cell 1)
