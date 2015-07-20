{-# LANGUAGE FlexibleInstances #-}
module Renderer.Field where

import Prelude 
import Data.Maybe(fromJust)
import World.Data
import World.Field.Field
import World.Field.Entities
import Renderer.Renderer
import Renderer.Character
import qualified FreeGame as F
import qualified Data.Map as M
import Control.Monad
import Control.Lens
import Data.Maybe (fromJust)

import Control.Parallel.Strategies

import qualified Data.Strict.Tuple as ST

import Debug.Trace

class FieldObjectR a where
    clip :: a -> Coord -> FieldData ->  F.Frame ()

tileMaps :: FieldData ->
            Coord ->  -- viewpoint
            (Coord -> Double) -> -- scaler
            F.Frame ()
tileMaps fd vp sfunc =
    forM_ (map (\t -> let c = t^.state^.cell 
                          bmp = cellBMP t
                          trans = fieldPosition vp c center
                      in ( trans
                         , (sfunc trans) / cellStatic * 1.08
                         , bmp 
                         ) 
                ) (fd^.fieldTips :: [CellTip]) 
          ) $ \(crd, s, bmp) -> F.translate crd $ F.scale (V2 s s) $ F.bitmap bmp


instance FieldObjectR Character where
    clip c vp f = 
        let elapsed = c^.charaState^.cellState^.elapsedFrames
            cdir = c^.charaState^.direct
            abpos = charaPos vp c
            action = c^.charaState^.acting
            cc = c^.charaState^.cellState^.cell
        in do 
          when (action == Whirlslash) $ F.color F.red $ fillCells vp $ worldWrapM (aroundCells 1) cc
          F.translate abpos $ F.bitmap $ cellBMP c

