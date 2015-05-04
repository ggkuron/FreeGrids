module World.Command where

import World.Data
import qualified Data.Map as M


data MoveCommand = Nuetral | Move Direct | Stop Direct deriving (Eq, Show, Ord)
data EffectCommand = Evolve | ENothing deriving (Eq, Show, Ord)

data ActionCommand = ActionCommand 
                   { moveCommand :: MoveCommand
                   , effectCommand :: EffectCommand
                   }
 

type Commands = M.Map Cell ActionCommand
