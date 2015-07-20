{-# LANGUAGE TemplateHaskell #-}
module World.Field.Entity.Entity
( module World.Field.Entity.Entity
, module Control.Lens 
) where

import World.Data
import Control.Lens 
import Paths_grids
import qualified FreeGame as F


import Control.DeepSeq

F.loadBitmapsWith [|getDataFileName|] "../../../../images"

data TipType = TIP_AMI
             | TIP_Grass
             | TIP_Player
             deriving (Show, Eq)

data CellProps = CellProps 
               { _block :: Bool 
               , _tipType :: TipType
               } deriving (Show, Eq)

makeLenses ''CellProps

instance NFData CellProps where
    rnf cp = cp `seq` ()

data CellState = CellState 
               { _cell :: WorldCell
               , _pos :: RCoord 
               , _elapsedFrames :: Int 
               } deriving (Show)

makeLenses ''CellState

data CellTip = CellTip { _props :: CellProps
                       , _state :: CellState
                       } deriving (Show)


makeLenses ''CellTip

class CellEntity e where
        celltip :: e -> CellTip

class CellEntityRender e where
        cellBMP :: e -> F.Bitmap

instance CellEntity CellTip where
        celltip = id

instance NFData CellState where
    rnf cs = cs `seq` rnf (cs^.cell) `seq` ()

instance NFData CellTip where
    rnf ci = rnf (ci^.props) `seq` rnf (ci^.state) `seq` ()

instance CellHolder CellTip where
    cellValue ci = cellValue  $ ci^.state^.cell


