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

data CharaAction = Stopping | Walking Direct | Whirlslash deriving (Eq, Show, Ord)

data CellProps = CellProps 
               { _movable :: Bool 
               , _block :: Bool 
               }
makeLenses ''CellProps

data CellState = CellState 
               { _cell :: Cell -- 位置
               , _pos :: RCoord -- Cellからの相対位置
               , _elapsedFrames :: Int -- 現在の状況での経過フレーム
               }
makeLenses ''CellState
