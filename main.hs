{-# LANGUAGE ImplicitParams, OverloadedStrings #-}
module Main where
import FreeGame
import Data.IORef
import Control.Monad (liftM)
-- import Data.List
import Data.Maybe (catMaybes)
-- import Control.Applicative

defaultWidth, defaultHeight :: Double
defaultWidth = 640
defaultHeight = 480

newtype Pair a b = Pair (a, b) deriving (Show,Eq)
instance (Num a, Num b) => Num(Pair a b) where 
    Pair (a,b) + Pair (c,d) = Pair (a+c, b+d)
    Pair (a,b) * Pair (c,d) = Pair (a*c, b*d)
    Pair (a,b) - Pair (c,d) = Pair (a-c, b-d)
    abs (Pair (a,b)) = Pair (abs a, abs b)
    signum (Pair (a,b)) = Pair (signum a, signum b)
    fromInteger i = Pair (fromInteger i, fromInteger i)

type Cell = Pair Int Int
type Point = Pair Double Double
data CellBox = CellBox Cell Cell

main :: IO (Maybe a)
main = runGame Windowed (BoundingBox 0 0 defaultWidth defaultHeight) $ do
    font <- embedIO $ loadFont "VL-Gothic-Regular.ttf"
    let ?font = font
    let  cell = Pair (3,3) 

    mainLoop cell

mainLoop :: (?font :: Font) => Cell -> Game a
mainLoop cell = do
    color black 
        $ translate (V2 50 50) 
        $ text ?font 30 "Free World"
    color black 
        $ translate (V2 150 150) 
        $ text ?font 30 $ show cell

    ownCell cell

type Rectangle = (Double, Double, Double, Double)

-- ownCell :: (Monad f, Monad m, Picture2D m
--            ,Control.Monad.Free.Class.MonadFree f (t m)
--            , Control.Monad.Trans.Class.MonadTrans t)
--            => Cell -> t m any
ownCell :: Cell -> Game a
ownCell cell =  do
    let origin = V2 0 0
        cell_long = 40
        thick = 2
        whole_rect = (0,0,defaultWidth, defaultHeight)

    io_cell <- embedIO $ newIORef cell

    foreverFrame $ do

        j <- keyChar 'J'
        k <- keyChar 'K'
        h <- keyChar 'H'
        l <- keyChar 'L'
        q <- keyChar 'Q'
    
        let dirs = catMaybes [if j then Just DOWN else Nothing
                             ,if k then Just UP else Nothing
                             ,if h then Just LEFT else Nothing
                             ,if l then Just RIGHT else Nothing]
        embedIO $ modifyIORef io_cell $ flip (foldr (flip adjacentCell)) dirs
    
        cell' <- embedIO $ readIORef io_cell

        color yellow 
            $ thickness thick 
            $ renderGrids whole_rect cell_long
        color red 
            $ fillCell origin cell_long cell'

renderGrids (x0, y0, w, h) interval = do
    zipWithM_ drawLine
            [V2 (x0 + p) y0      | p <- [0,interval .. w]] 
            [V2 (x0 + p) (y0 + h)| p <- [0,interval ..]] 
    zipWithM_ drawLine
            [V2 x0 (y0 + p)| p <- [0,interval .. h]] 
            [V2 (x0 + w) (y0 + p)| p <- [0,interval ..]] 
           where drawLine x y = line [x, y]

rectCell :: Vec2 -> Double -> Cell -> [V2 Double]
rectCell origin long cell = [ cornerPoint' UP LEFT $ cell
                            , cornerPoint' DOWN LEFT $ cell 
                            , cornerPoint' DOWN RIGHT $ cell 
                            , cornerPoint' UP RIGHT $ cell 
                            ]
                         where 
                         cornerPoint' = cornerPoint origin long

renderCellOutline :: Picture2D p => Vec2 -> Double -> Direct -> Cell -> p ()
renderCellOutline origin long UP    cell = line [cornerPoint origin long UP LEFT cell,   cornerPoint origin long UP RIGHT cell]
renderCellOutline origin long DOWN  cell = line [cornerPoint origin long DOWN LEFT cell, cornerPoint origin long DOWN RIGHT cell]
renderCellOutline origin long RIGHT cell = line [cornerPoint origin long UP RIGHT cell,  cornerPoint origin long DOWN RIGHT cell]
renderCellOutline origin long LEFT  cell = line [cornerPoint origin long UP LEFT cell,   cornerPoint origin long DOWN LEFT cell]

cornerPoint :: Vec2 -> Double -> Direct -> Direct -> Cell -> V2 Double
cornerPoint origin long UP    LEFT  (Pair (r,c)) = origin + V2 (fromIntegral c * long) (fromIntegral r * long)     
cornerPoint origin long LEFT  UP    (Pair (r,c)) = origin + V2 (fromIntegral c * long) (fromIntegral r * long)     
cornerPoint origin long DOWN  LEFT  (Pair (r,c)) = origin + V2 (fromIntegral c * long) (fromIntegral (r+1) * long)     
cornerPoint origin long LEFT  DOWN  (Pair (r,c)) = origin + V2 (fromIntegral c * long) (fromIntegral (r+1) * long)     
cornerPoint origin long RIGHT DOWN  (Pair (r,c)) = origin + V2 (fromIntegral (c+1) * long) (fromIntegral (r+1) * long) 
cornerPoint origin long DOWN  RIGHT (Pair (r,c)) = origin + V2 (fromIntegral (c+1) * long) (fromIntegral (r+1) * long) 
cornerPoint origin long UP    RIGHT (Pair (r,c)) = origin + V2 (fromIntegral (c+1) * long) (fromIntegral r * long) 
cornerPoint origin long RIGHT UP    (Pair (r,c)) = origin + V2 (fromIntegral (c+1) * long) (fromIntegral r * long) 
cornerPoint origin long _     _     (Pair (r,c)) = undefined

fillCell :: Picture2D p => Vec2 -> Double -> Cell -> p () 
fillCell origin long cell = polygon $ rectCell origin long cell 

strokeCell :: Picture2D p => Vec2 -> Double -> Cell -> p ()
strokeCell origin long cell = polygonOutline $ rectCell origin long cell 

fillCells :: (Monad m, Picture2D m) => Vec2 -> Double -> [Cell] -> m ()
fillCells origin long cells = mapM_ fillCell' cells 
    where fillCell' = fillCell origin long

strokeCells :: (Monad m, Picture2D m) => Vec2 -> Double -> [Cell] -> m ()
strokeCells origin long cells = mapM_ render cells
    where renderCellOutline' = renderCellOutline origin long
          render c = let adjacents = adjacentDirections cells c
                     in mapM_ (\d -> renderCellOutline' d c) adjacents

data Direct = UP | DOWN | LEFT | RIGHT

allDirection :: [Direct]
allDirection = [UP, DOWN, LEFT, RIGHT]

type Board = [Cell]

adjacentCell :: Cell -> Direct -> Cell
adjacentCell (Pair (r,c)) UP    = Pair (r-1,c)
adjacentCell (Pair (r,c)) DOWN  = Pair (r+1,c)
adjacentCell (Pair (r,c)) LEFT  = Pair (r,c-1)
adjacentCell (Pair (r,c)) RIGHT = Pair (r,c+1)

adjacentDirection :: Cell -> Cell -> Maybe Direct
adjacentDirection (Pair (bc,br)) (Pair (pc,pr)) | (bc,br) == (pc-1,pr) = Just UP   
                                                | (bc,br) == (pc+1,pr) = Just DOWN 
                                                | (bc,br) == (pc,pr-1) = Just LEFT 
                                                | (bc,br) == (pc,pr+1) = Just RIGHT
                                                | otherwise = Nothing

adjacentDirections :: Board -> Cell -> [Direct]
adjacentDirections board cell = catMaybes $ map (adjacentDirection cell) board

peripheralCells :: Cell -> [Cell]
peripheralCells cell = map (adjacentCell cell) allDirection

adjacentCells :: Board -> Cell -> [Cell]
adjacentCells board cell = filter (`elem` board) (peripheralCells cell)

