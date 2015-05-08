{-# LANGUAGE TemplateHaskell #-}
module Renderer.Renderer where

import World.Data
import Control.Lens
import Control.Monad
import qualified FreeGame as F


-- edgeIn :: F.Affine p => Double -> Coord -> MapCell -> FieldCell -> p a -> p a
-- edgeIn m o mc c = F.translate $ cornerPoint o UpperLeft mc c 

renderGrids :: Rect -> Double -> F.Frame ()
renderGrids rect interval = renderStripeH rect interval 
                                >> renderStripeV rect interval

renderStripeV :: Rect -> Double -> F.Frame ()
renderStripeV (x0, y0, w, h) interval  = zipWithM_ (\x y -> F.line [x,y])
            [F.V2 (x0 + p)  y0      | p <- [0,interval .. w]] 
            [F.V2 (x0 + p) (y0 + h) | p <- [0,interval ..]] 

renderStripeH :: Rect -> Double -> F.Frame ()
renderStripeH (x0, y0, w, h) interval = zipWithM_ (\x y -> F.line [x,y])
            [F.V2 x0       (y0 + p) | p <- [0,interval .. h]] 
            [F.V2 (x0 + w) (y0 + p) | p <- [0,interval ..]] 
-- renderStripeV (x0, y0, w, h) interval = zipWithM_ (\x y -> F.line [x,y])
--             [F.V2 (stripeVXModifier (x0 + p) (x0 + w) 0.3)  y0 | p <- [0, interval .. w]] 
--             [F.V2 (x0 + p) (y0 + h) | p <- [0, interval ..]] 


renderCellOutline :: F.Picture2D p => Double -> Coord -> Direct -> SizeTuple -> MapCell -> FieldCell -> p ()
renderCellOutline m origin dir st mc c = F.line $ map (\edge -> cornerPoint origin edge st mc c) $ directLineEdge dir

fillCell :: F.Picture2D p => Double -> Coord -> SizeTuple -> MapCell -> FieldCell -> p () 
fillCell m origin st mc fc = F.polygon $ rectCell m origin st mc fc

strokeCell :: F.Picture2D p => Double -> Coord -> SizeTuple -> MapCell -> FieldCell -> p ()
strokeCell m origin st mc fc = F.polygonOutline $ rectCell m origin st mc fc

-- strokeCells :: (Monad p, F.Picture2D p) => F.Vec2 -> SizeTuple -> Double -> Double -> [Cell] -> p ()
-- strokeCells origin m long cs = mapM_ render cs
--      where renderCellOutline' = renderCellOutline m origin long
--            adjacents c = adjacentDirections cs c
--            render c =  mapM_ (\d -> renderCellOutline' d c) (adjacents c)


fillCells :: (Monad p, F.Picture2D p) => Double -> Coord -> SizeTuple -> MapCell -> [FieldCell] -> p ()
fillCells m origin st mc cs = forM_ cs $ fillCell m origin st mc


