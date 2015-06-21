{-# LANGUAGE FlexibleInstances #-}
module Renderer.Field where

import Prelude 
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

import Control.Parallel.Strategies

import Debug.Trace

class FieldMapR a where
    tileMaps :: a ->
                Coord ->  -- viewpoint
                (Coord -> Double) -> -- scaler
                F.Frame ()


class FieldObjectR a where
    clip :: (FieldMapI f) => a -> Coord -> f ->  F.Frame ()


tileMapInner :: FieldMap -> Coord -> (Coord -> Double) -> [(Coord, Double, F.Bitmap)]
tileMapInner f vp sfunc =
    let bp = backpict f
        ms = mapSize f
        mi = mapIndex f
     in concat (map (\crange -> let bmp = fromJust $ M.lookup crange bp
                                 in map (\(SizedBlock15x15 fc) -> let trans = fieldPosition vp ms mi fc center
                                                                   in ( trans
                                                                      , (sfunc trans) / cellStatic * 1.08
                                                                      , bmp 
                                                                      ) 
                                        ) [(fst crange)..(snd crange)] `using` parList rdeepseq
                     ) (M.keys bp) `using` parList rdeepseq 
               )
 

instance FieldMapR FieldMap where
    tileMaps f vp sfunc = forM_ (tileMapInner f vp sfunc)
                          $ \(crd, s, bmp) -> F.translate crd $ F.scale (V2 s s) $ F.bitmap bmp

        -- let bp = backpict f
        -- in forM_ (M.keys bp) $
        --    \crange -> mapM (\(SizedBlock15x15 fc) -> let transVal = fieldPosition vp (mapSize f) (mapIndex f) fc center;
        --                                                  scale = (sfunc transVal) / cellStatic * 1.08;
        --                                               in F.translate transVal $ F.scale (V2 scale scale) $ F.bitmap (fromJust $ M.lookup crange bp)
        --               ) [(fst crange)..(snd crange)]
instance FieldObjectR Character where
    clip (props, state) vp f = 
        let elapsed = state^.cellState^.elapsedFrames
            cdir = state^.direct
            obj_cells = map ((^.fieldCell).(Prelude.snd)) (mapObjects f)
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

