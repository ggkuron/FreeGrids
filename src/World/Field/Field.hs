{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}


module World.Field.Field where

import World.Data
import World.Field.Entity.Entity
import World.Field.Entity.Character
import World.Command
import qualified Data.Map as M
import qualified FreeGame as F

import Control.DeepSeq

newtype SizedBlock15x15 a = SizedBlock15x15 a
                          deriving(Eq,Ord,Show,Functor)

type SizedCell15x15 = SizedBlock15x15 FieldCell

-- Ix a => SizeBlock a 
-- でblockSize不要 

class SizedBlock a where
    createBlock :: SizeTuple -> a
    blockSize :: a -> SizeTuple

instance SizedBlock (SizedBlock15x15 FieldCell) where
    blockSize a = SizeTuple (15 :!: 15)
    createBlock (SizeTuple t) = SizedBlock15x15 $ fcell t

instance CellHolder SizedCell15x15 where
    cellValue (SizedBlock15x15 (FieldObject c)) = c
    holdering c = SizedBlock15x15 $ FieldObject c

-- sbRange sb = spanRange (fcell(0 :!: 0)) (blockSizeCell sb)
--     where 
--       blockSizeCell :: (SizedBlock a) => a -> FieldCell
--       blockSizeCell sb = let (SizeTuple t) = blockSize sb
--                              in fcell t

sbSucc sb | cellRow cv == sizeTupleRow (blockSize sb) = createBlock' (adjacentCell DOWN cv)
          | otherwise = createBlock' $ adjacentCell RIGHT cv
  where cv = cellValue sb
        createBlock' (Cell t) = createBlock (SizeTuple t)
        
sbFromEnum sb = let (SizeTuple (_ :!: mc)) = blockSize sb
                    Cell (r :!: c) = cellValue sb
                 in r * mc + c

-- instance Ranged (SizedBlock15x15 FieldCell) FieldCell where
--     range = sbRange
--     rangedValue (SizedBlock15x15 fc) = fc

instance Enum SizedCell15x15 where
    succ = sbSucc
    fromEnum = sbFromEnum
    toEnum i = let (d, m) = divMod i 15
                in SizedBlock15x15 $ fcell (d :!: m)

data FieldMap = FieldMap 
              { fieldIndex :: MapCell
              , mobj :: [(CellProps,CellState)] -- (Enty, 初期State)
              , mchr :: [(CharaProps,CharaState)] -- (Enty, 初期State)
              , backpict :: M.Map (SizedCell15x15 :!: SizedCell15x15) F.Bitmap
              , mapsize :: SizeTuple
              }

instance (NFData FieldMap) where
    rnf a = a `seq` ()

class FieldMapI a where
    mapIndex :: a -> MapCell
    mapObjects :: a -> [(CellProps, CellState)]
    mapCharacters :: a -> [(CharaProps,CharaState)] 
    mapSize :: a -> SizeTuple

class FieldSheet a where
    outerEdgeCell :: a -> MapCell -> Direct -> FieldCell -> FieldCell
    lookupFieldMap :: a -> MapCell -> Maybe FieldMap

instance FieldMapI FieldMap where
    mapIndex = fieldIndex 
    mapObjects = mobj 
    mapCharacters = mchr
    mapSize = mapsize
    
class FieldActor a where
    actOn :: (FieldMapI f, FieldSheet fs) => fs -> f -> ActionCommand -> a -> a
    effect :: a -> Commands -> Commands


