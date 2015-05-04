{-# LANGUAGE MultiParamTypeClasses #-}

module World.Field.Field where

import World.Data
import World.Field.Entity.Entity
import World.Field.Entity.Character
import World.Command
import qualified Data.Map as M
import qualified FreeGame as F

newtype SizedBlock25x25 = SizedBlock25x25 Cell
                          deriving(Eq,Show,Ord)

class SizedBlock a where
    cellValue :: a -> Cell
    createBlock :: Cell -> a
    blockSize :: a -> SizeTuple

instance SizedBlock SizedBlock25x25 where
    cellValue (SizedBlock25x25 c) = c
    blockSize a = SizeTuple (25,25)
    createBlock = SizedBlock25x25 

blockSizeCell :: (SizedBlock a) => a -> Cell
blockSizeCell sb = sizeTupleCell $ blockSize sb

sbRange sb = spanRange (Cell(0,0)) (blockSizeCell sb)
sbRangedValue = cellValue

sbSucc sb | mr == cellRow (cellValue sb)  = createBlock $ adjacentCell cv DOWN
          | otherwise = createBlock $ adjacentCell cv RIGHT
  where mr = cellRow $ blockSizeCell sb ::Int 
        cv = cellValue sb
sbFromEnum sb = r * mc + c
  where (SizeTuple (_, mc)) = blockSize $ sb
        Cell (r, c) = cellValue sb


instance Ranged SizedBlock25x25 Cell where
    range = sbRange
    rangedValue = sbRangedValue

instance Enum(SizedBlock25x25) where
    succ = sbSucc
    fromEnum = sbFromEnum
    toEnum i = SizedBlock25x25 $Cell (divMod i 25)

data FieldMap = FieldMap 
              { fieldIndex :: Cell
              , mobj :: [(CellProps,CellState)] -- (Enty, 初期State)
              , mchr :: [(CharaProps,CharaState)] -- (Enty, 初期State)
              , backpict :: M.Map (SizedBlock25x25 , SizedBlock25x25) F.Bitmap
              , mapsize :: SizeTuple
              }

class FieldMapI a where
    mapIndex :: a -> Cell
    mapObjects :: a -> [(CellProps, CellState)]
    mapCharacters :: a -> [(CharaProps,CharaState)] 
    mapSize :: a -> SizeTuple

instance FieldMapI FieldMap where
    mapIndex = fieldIndex 
    mapObjects = mobj 
    mapCharacters = mchr
    mapSize = mapsize

class FieldObject a where
    actOn :: ActionCommand -> a -> a
    effect :: a -> Commands -> Commands
