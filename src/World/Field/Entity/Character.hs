{-# LANGUAGE TemplateHaskell #-}
module World.Field.Entity.Character where

import World.Data
import World.Command
import World.Field.Entity.Entity
import qualified Data.Map as M
import qualified FreeGame as F
import Control.Lens 

data CharaProps = CharaProps 
                { _cellProps :: CellProps
                , _fourSides :: M.Map Direct [F.Bitmap]
                }
makeLenses ''CharaProps

data CharaAction = Stopping | Walking Direct | Whirlslash deriving (Eq, Show, Ord)

data CharaState = CharaState 
                { _hp :: Int
                , _direct :: Direct
                , _acting :: CharaAction
                , _cellState :: CellState
                }
makeLenses ''CharaState

type Character = (CharaProps, CharaState)

class CharaStateI a where
    charaDirection :: a -> Direct
    charaAction :: a -> CharaAction

instance CharaStateI CharaState where
    charaDirection = _direct
    charaAction = _acting

