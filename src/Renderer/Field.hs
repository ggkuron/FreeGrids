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
            F.Frame ()
tileMaps fd vp =
    mapM_  (\(crd, s, bmp) -> F.translate crd $ F.scale (V2 s s) $ F.bitmap bmp)
           $ flip map (fd^.fieldTips) $
               (\t -> let trans = fieldPosition vp (t^.state^.cell) center
                       in (trans , (cellLong trans) / cellStatic * 1.02, (cellBMP t)))

instance FieldObjectR Character where
    clip c vp f = 
        let action = c^.charaState^.acting
        in do 
          when (action == Whirlslash) 
              $ F.color F.red $ fillCells vp $ wrapF (aroundCells 1) (c^.charaState^.cellState^.cell)
          F.translate (charaPos vp c) $ F.bitmap $ cellBMP c

