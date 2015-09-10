{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}


module World.Field.Field where

import Prelude hiding(min, max)
import World.Data
import World.Field.Entity.Entity
import World.Field.Entity.Character
import World.Command

import Control.Parallel.Strategies
import Control.DeepSeq


-- data FieldRawData = FieldRawData
--                   { fieldIndex :: BCell
--                   , mobj :: [CellTip] -- (Enty, 初期State)
--                   , mchr :: [Character] -- (Enty, 初期State)
--                   }

data FieldData = FieldData  
               { _fieldTips :: [CellTip] 
               , _fieldCharacters ::  [Character] 
               }

makeLenses ''FieldData

collectTips :: FieldData -> [CellTip]
collectTips f = (f^.fieldTips) ++ (map celltip $ f^.fieldCharacters)

aroundField :: WorldIndex w => w -> w -> FieldData -> FieldData
aroundField sz i f = let (Cell(minr:!:minc)) = (from sz) - (from i)
                         (Cell(maxr:!:maxc)) = (from sz) + (from i)
                         comp (Cell(r:!:c)) =  minr < r && maxr > r && minc < c && maxc > c
                         in FieldData (filter (\t -> comp $ from (t^.state^.cell)) (f^.fieldTips))
                                       (filter (\t -> comp $ from (t^.charaState^.cellState^.cell)) (f^.fieldCharacters))

blockCheck :: FieldData -> WorldCell -> Bool
blockCheck f c = any (\t -> t^.props^.block && adjacent (t^.state^.cell) c)
                     $ (f^.fieldTips) ++ (map celltip $  f^.fieldCharacters)
    where adjacent a b = 1 == (cellDistance (from a) (from b))


-- instance NFData FieldRawData where
--     rnf fm = rnf (mobj fm) `seq` rnf (mchr fm) `seq` () 

class FieldActor a where
    actOn :: FieldData -> ActionCommand -> a -> a
    effect :: a -> Commands -> Commands

