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


data FieldRawData = FieldRawData
              { fieldIndex :: BCell
              , mobj :: [CellTip] -- (Enty, 初期State)
              , mchr :: [Character] -- (Enty, 初期State)
              }

data FieldData = FieldData  
               { _fieldTips :: [CellTip] 
               , _fieldCharacters ::  [Character] 
               }

makeLenses ''FieldData

blockCheck :: FieldData -> WorldCell -> Bool
blockCheck f c = any (\t -> t^.props^.block && adjacent (t^.state^.cell) c)
                     $ (f^.fieldTips) ++ (map celltip $  f^.fieldCharacters)
    where adjacent a b = 1 == (cellDistance (cellValue a) (cellValue b))



instance NFData FieldRawData where
    rnf fm = rnf (mobj fm) `seq` rnf (mchr fm) `seq` () 

class FieldActor a where
    actOn :: FieldData -> ActionCommand -> a -> a
    effect :: a -> Commands -> Commands

