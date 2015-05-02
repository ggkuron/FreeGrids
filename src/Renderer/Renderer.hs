{-# LANGUAGE TemplateHaskell #-}
module Renderer.Renderer where

import World.Data
import Control.Lens
import Control.Monad
import qualified FreeGame as F


coordVec :: Coord -> F.Vec2
coordVec (x, y) =  F.V2 (fromIntegral x) (fromIntegral y)

normalMapping :: Double  -> Cell -> F.Vec2
normalMapping long (Cell (r,c)) = F.V2 (fromIntegral c * long) (fromIntegral r * long) 


rectCell :: Double -> SizeTuple -> F.Vec2 -> Double -> Cell -> [F.Vec2]
rectCell m msize origin long c = map (\e -> cornerPoint msize origin long e c (normalTrans m)) allEdge

edgeIn :: F.Affine p => Double -> SizeTuple -> F.Vec2 -> Double -> Cell -> p a -> p a
edgeIn m sm o l c = F.translate $ cornerPoint sm o l UpperLeft c (normalTrans m)

cornerPoint :: SizeTuple -> F.Vec2 -> Double -> Edge -> Cell -> (SizeTuple -> F.Vec2 -> F.Vec2) -> F.Vec2
cornerPoint sm vp long edge c trans = trans' $ vp + normalMapping long (celledge c edge)
    where
        trans' = trans sm :: F.Vec2 -> F.Vec2

getRCoord :: RCoord -> F.Vec2
getRCoord (rx, ry) = F.V2 (fromR rx) (fromR ry)
    where 
      sliderSize' = fromIntegral.sliderSize 
      persent rc = fromIntegral $ rc^.percent^.rangeInsideValue :: Double
      fromR :: Slider -> Double
      fromR rc = sliderSize' rc  * (persent rc / 100) + cellStatic / 2


picPos :: Double -> SizeTuple -> F.Vec2 -> Cell -> RCoord -> F.Vec2
picPos m sm o c rc = cornerPoint sm o cellStatic UpperLeft c (normalTrans m) + getRCoord rc

yTrans = \y h -> (h - y) / h

normalTrans :: Double ->  SizeTuple -> F.Vec2 ->  F.Vec2 
normalTrans = transBase yTrans

stripeVXModifier :: Fractional a => a -> a -> a -> a
stripeVXModifier x w m = x + ((w / 2 - x) * m)

transBase :: (Double -> Double ->  Double) -> Double -> SizeTuple -> F.Vec2 -> F.Vec2
transBase acl m sm (F.V2 x y) = F.V2 (stripeVXModifier x mWidth (ydiff * m)) y
    where
        (F.V2 mr mc) = coordVec.maxCoord $ sm :: F.Vec2
        mWidth = defaultWidth
        yWidth = mr * cellStatic
        ydiff = acl y yWidth 


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


renderCellOutline :: F.Picture2D p => Double -> SizeTuple -> F.Vec2 -> Double -> Direct -> Cell -> p ()
renderCellOutline m sm origin long dir c = F.line $ map (\edge -> cornerPoint sm origin long edge c (normalTrans m))  $ directLineEdge dir

fillCell :: F.Picture2D p => Double -> SizeTuple -> F.Vec2 -> Double -> Cell -> p () 
fillCell m sm origin long c = F.polygon $ rectCell m sm origin long c 

strokeCell :: F.Picture2D p => Double -> SizeTuple -> F.Vec2 -> Double -> Cell -> p ()
strokeCell m sm origin long c = F.polygonOutline $ rectCell m sm origin long c 

-- strokeCells :: (Monad p, F.Picture2D p) => F.Vec2 -> SizeTuple -> Double -> Double -> [Cell] -> p ()
-- strokeCells origin m long cs = mapM_ render cs
--      where renderCellOutline' = renderCellOutline m origin long
--            adjacents c = adjacentDirections cs c
--            render c =  mapM_ (\d -> renderCellOutline' d c) (adjacents c)


fillCells :: (Monad p, F.Picture2D p) => Double -> SizeTuple ->  F.Vec2 -> Double -> [Cell] -> p ()
fillCells m sm origin long cs = forM_ cs $ fillCell m sm origin long


