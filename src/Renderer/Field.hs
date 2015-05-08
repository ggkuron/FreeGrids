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

import Debug.Trace

class FieldMapR a where
    tileMaps :: a ->
                Coord ->  -- viewpoint
                (Coord -> Double) -> -- scaler
                F.Frame ()

class FieldObjectR a where
    clip :: (FieldMapI f) => a -> Coord -> f ->  F.Frame ()

instance FieldMapR FieldMap where
    tileMaps f vp sfunc =
        let bp = backpict f
            mc = mapIndex f
        in forM_ (M.keys bp) $
           \crange -> let b = fromJust (M.lookup crange bp) :: F.Bitmap
                      in forM_ [(fst crange)..(snd crange)] $
                               \rc -> let (SizedBlock15x15 fc) = rc -- size specific
                                          transVal = fieldPosition vp (mapSize f) mc fc center
                                          scale = (sfunc transVal) / cellStatic * 1.08
                                      in F.translate transVal $ F.scale (V2 scale scale) $ F.bitmap b

instance FieldObjectR Character where
    clip (props, state) vp f = 
        let elapsed = state^.cellState^.elapsedFrames
            cdir = state^.direct
            obj_cells = map ((^.fieldCell).snd) (mapObjects f)
            fc  = state^.cellState^.fieldCell :: FieldCell
            mc = state^.cellState^.mapCell :: MapCell
            p = state^.cellState^.pos :: RCoord
            abpos = fieldPosition vp (mapSize f) mc fc p
            fside = props^.fourSides
            action = charaAction state
            pickUp :: CharaAction -> F.Bitmap
            pickUp Stopping = (fromJust ( M.lookup cdir fside)) !! 0
            pickUp Whirlslash = (fromJust ( M.lookup cdir fside)) !! 0
            pickUp (Walking dir) = fromJust (M.lookup dir fside) !! (index elapsed) 
                 where index et | et < 8 = 1
                                | et < 16 = 2
                                | et < 24 = 3
                                | otherwise = 4
        in do 
          when (action == Whirlslash) $ F.color F.red $ fillCells transMod vp (mapSize f) mc $ peripheralFieldCells fc 1
          F.translate abpos $ F.bitmap $ pickUp action

