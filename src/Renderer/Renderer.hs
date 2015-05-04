{-# LANGUAGE TemplateHaskell #-}
module Renderer.Renderer where

import World.Data
import Control.Lens
import Control.Monad
import qualified FreeGame as F


-- picPos :: Double -> SizeTuple -> Coord -> Cell -> RCoord -> Coord
-- picPos m sm o c rc = cornerPoint sm o cellStatic UpperLeft c (normalTrans m) + getRCoord rc
--
edgeIn :: F.Affine p => Double -> SizeTuple -> Coord -> Int -> Cell -> p a -> p a
edgeIn m sm o l c = F.translate $ cornerPoint sm o l UpperLeft c (normalTrans m)

renderGrids :: Rect -> Double -> F.Frame ()
renderGrids rect interval = renderStripeH rect interval 
                                >> renderStripeV rect interval

renderStripeV' :: Rect -> Double -> F.Frame ()
renderStripeV' (x0, y0, w, h) interval  = zipWithM_ (\x y -> F.line [x,y])
            [F.V2 (x0 + p)  y0      | p <- [0,interval .. w]] 
            [F.V2 (x0 + p) (y0 + h) | p <- [0,interval ..]] 

renderStripeH :: Rect -> Double -> F.Frame ()
renderStripeH (x0, y0, w, h) interval = zipWithM_ (\x y -> F.line [x,y])
            [F.V2 x0       (y0 + p) | p <- [0,interval .. h]] 
            [F.V2 (x0 + w) (y0 + p) | p <- [0,interval ..]] 
renderStripeV (x0, y0, w, h) interval = zipWithM_ (\x y -> F.line [x,y])
            [F.V2 (stripeVXModifier (x0 + p) (x0 + w) 0.3)  y0 | p <- [0, interval .. w]] 
            [F.V2 (x0 + p) (y0 + h) | p <- [0, interval ..]] 


renderCellOutline :: F.Picture2D p => Double -> SizeTuple -> Coord -> Int -> Direct -> Cell -> p ()
renderCellOutline m sm origin long dir c = F.line $ map (\edge -> cornerPoint sm origin long edge c (normalTrans m))  $ directLineEdge dir

fillCell :: F.Picture2D p => Double -> SizeTuple -> Coord -> Int -> Cell -> p () 
fillCell m sm origin long c = F.polygon $ rectCell m sm origin long c 

strokeCell :: F.Picture2D p => Double -> SizeTuple -> Coord -> Int -> Cell -> p ()
strokeCell m sm origin long c = F.polygonOutline $ rectCell m sm origin long c 

-- strokeCells :: (Monad p, F.Picture2D p) => F.Vec2 -> SizeTuple -> Double -> Double -> [Cell] -> p ()
-- strokeCells origin m long cs = mapM_ render cs
--      where renderCellOutline' = renderCellOutline m origin long
--            adjacents c = adjacentDirections cs c
--            render c =  mapM_ (\d -> renderCellOutline' d c) (adjacents c)


fillCells :: (Monad p, F.Picture2D p) => Double -> SizeTuple ->  Coord -> Int -> [Cell] -> p ()
fillCells m sm origin long cs = forM_ cs $ fillCell m sm origin long


