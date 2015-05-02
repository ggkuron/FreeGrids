module World.Field.Entity.Entity where

import World.Data
import World.Command
import qualified Data.Map as M
import qualified FreeGame as F

type CellObj = (CellProps, CellState)

data CharaAction = Stopping | Walking Direct | Whirlslash deriving (Eq, Show, Ord)

data CellProps = CellProps 
               { _movable :: Bool 
               , _block :: Bool 
               }

data CellState = CellState 
               { _cell :: Cell -- 位置
               , _pos :: RCoord -- Cellからの相対位置
               , _elapsedFrames :: Int -- 現在の状況での経過フレーム
               }


