{-# LANGUAGE TemplateHaskell #-}
module World.Field.Entity.Entity
( module World.Field.Entity.Entity
, module Control.Lens
) where

import World.Data
import World.Command
import Control.Lens
import qualified Data.Map as M
import qualified FreeGame as F


data CellProps = CellProps 
               { _movable :: Bool 
               , _block :: Bool 
               }

makeLenses ''CellProps

data CellState = CellState -- 位置
               { _mapCell :: MapCell
               , _fieldCell :: FieldCell 
               , _pos :: RCoord -- Cellからの相対位置
               , _elapsedFrames :: Int -- 現在の経過フレーム
               }


makeLenses ''CellState

cellIndex :: CellState -> (MapCell, FieldCell)
cellIndex cs = (cs^.mapCell, cs^.fieldCell)


