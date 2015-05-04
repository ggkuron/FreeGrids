{-# LANGUAGE FlexibleInstances #-}
module Renderer.Field where

import Control.Monad
import Data.Maybe(fromJust)
import World.Data
import World.Field.Field
import World.Field.Entities
import Renderer.Renderer
import qualified FreeGame as F
import qualified Data.Map as M
import Control.Monad
import Control.Lens
import Data.Maybe (fromJust)

class FieldMapR a where
    tileMaps :: a -> Coord -> (SizeTuple -> Coord -> Coord) -> F.Frame ()

class FieldObjectR a where
    clip :: (FieldMapI f) => a -> Coord -> f ->  F.Frame ()

instance FieldMapR FieldMap where
    tileMaps f vp trans =
        let bp = backpict f
        in forM_ (M.keys bp) $
           \crange -> let b = fromJust (M.lookup crange bp) :: F.Bitmap
                      in forM_ [(fst crange)..(snd crange)] $
                               \rc -> let msize = mapSize f
                                          transVal = trans msize $ fieldPosition transMod msize vp (cellValue rc) center
                                      in F.translate transVal (F.bitmap b)

instance FieldObjectR Character where
    clip (props, state) vp f = 
        let fside = props^.fourSides
            action = charaAction state
            elapsed = state^.cellState^.elapsedFrames
            cdir = state^.direct
            mapsize = mapSize f
            obj_cells = map ((^.cell).snd) (mapObjects f)
            c = state^.cellState^.cell :: Cell
            p = state^.cellState^.pos :: RCoord
            abpos = fieldPosition transMod (mapSize f) vp c p
            pickUp :: CharaAction -> F.Bitmap
            pickUp Stopping = (fromJust ( M.lookup cdir fside)) !! 0
            pickUp Whirlslash = (fromJust ( M.lookup cdir fside)) !! 0
            pickUp (Walking dir) = fromJust (M.lookup dir fside) !! (index elapsed) 
                 where index et | et < 8 = 1
                                | et < 16 = 2
                                | et < 24 = 3
                                | otherwise = 4
        in do 
          when (action == Whirlslash) $ F.color F.red  $ fillCells transMod mapsize vp cellStatic $ peripheralCells c 1
          F.translate abpos $ F.bitmap $ pickUp action

cellLong :: (FieldMapI f) => Coord -> f -> Cell -> Double 
cellLong vp f c = let V2 x _ = vp + normalMapping cellStatic (celledge c UpperLeft)
                      SizeTuple (mr, _) = mapSize f
                in yTrans x (cellStatic * (fromIntegral mr))


