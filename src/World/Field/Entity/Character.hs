{-# LANGUAGE TemplateHaskell #-}
module World.Field.Entity.Character where

import World.Data
import World.Field.Entity.Entity
import Control.DeepSeq


data CharaProps = CharaProps 
                { _cellProps :: CellProps
                -- , _fourSides :: M.Map Direct [F.Bitmap]
                } 
makeLenses ''CharaProps

data CharaAction = Stopping | Walking | Whirlslash deriving (Eq, Show, Ord)

data CharaState = CharaState 
                { _hp :: Int
                , _direct :: Direct
                , _acting :: CharaAction
                , _cellState :: CellState
                } deriving Show
makeLenses ''CharaState

data Character = Character 
               { _charaProps :: CharaProps
               , _charaState :: CharaState
               }
makeLenses ''Character

newtype Player = Player 
               { _playerChara :: Character 
               } 
makeLenses ''Player
 

instance CellEntity Character where
    celltip c = CellTip (c^.charaProps^.cellProps) (c^.charaState^.cellState)

 

instance NFData CharaProps where
    rnf cp = rnf (cp^.cellProps) `seq` ()
instance NFData CharaState where
    rnf cs = cs^.hp `seq` cs^.direct `seq` cs^.acting `seq` cs^.cellState `seq` ()
instance NFData Character where
    rnf c = rnf (c^.charaProps) `seq` rnf (c^.charaState)

