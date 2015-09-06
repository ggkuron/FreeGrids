{-# LANGUAGE TemplateHaskell #-}
module Renderer.Renderer where

import World.Data
import Control.Lens
import Control.Monad
import qualified FreeGame as F
import Paths_grids

import Control.DeepSeq

class CellEntityRender e where
        cellBMP :: e -> F.Bitmap

F.loadBitmapsWith [|getDataFileName|] "../../static/images"

instance (NFData F.Bitmap) where
    rnf a = a `seq` ()

renderGrids :: Rect -> Double -> F.Frame ()
renderGrids rect interval = renderStripeH rect interval 
                                >> renderStripeV rect interval
  where
    renderStripeV :: Rect -> Double -> F.Frame ()
    renderStripeV (x0, y0, w, h) interval  = zipWithM_ (\x y -> F.line [x,y])
                [F.V2 (x0 + p)  y0      | p <- [0,interval .. w]] 
                [F.V2 (x0 + p) (y0 + h) | p <- [0,interval ..]] 
    
    renderStripeH :: Rect -> Double -> F.Frame ()
    renderStripeH (x0, y0, w, h) interval = zipWithM_ (\x y -> F.line [x,y])
                [F.V2 x0       (y0 + p) | p <- [0,interval .. h]] 
                [F.V2 (x0 + w) (y0 + p) | p <- [0,interval ..]] 

-- bridge view
-- renderStripeV (x0, y0, w, h) interval = zipWithM_ (\x y -> F.line [x,y])
--             [F.V2 (stripeVXModifier (x0 + p) (x0 + w) 0.3)  y0 | p <- [0, interval .. w]] 
--             [F.V2 (x0 + p) (y0 + h) | p <- [0, interval ..]] 


renderCellOutline :: F.Picture2D p => Double -> Coord -> Direct -> WorldCell -> p ()
renderCellOutline m origin dir mc = F.line $ map (\edge -> cornerPoint origin edge mc) $ directLineEdge dir

fillCell :: F.Picture2D p => Coord -> WorldCell -> p () 
fillCell origin mc = F.polygon $ rectCell origin mc

strokeCell :: F.Picture2D p => Coord -> WorldCell -> p ()
strokeCell origin mc = F.polygonOutline $ rectCell origin mc

-- strokeCells :: (Monad p, F.Picture2D p) => F.Vec2 -> SizeTuple -> Double -> Double -> [Cell] -> p ()
-- strokeCells origin m long cs = mapM_ render cs
--      where renderCellOutline' = renderCellOutline m origin long
--            adjacents c = adjacentDirections cs c
--            render c =  mapM_ (\d -> renderCellOutline' d c) (adjacents c)

fillCells :: (Monad p, F.Picture2D p) => Coord -> [WorldCell] -> p ()
fillCells origin cs = forM_ cs $ fillCell origin


