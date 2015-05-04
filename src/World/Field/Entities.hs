{-# LANGUAGE FlexibleInstances #-}
module World.Field.Entities
( module World.Field.Entity.Entity
, module World.Field.Entity.Character
, module World.Field.Entities
)
where

import World.Field.Field
import World.Field.Entity.Entity
import World.Field.Entity.Character

import World.Data
import World.Command
import World.Field.Entity.Entity

import qualified Data.Map as M
import qualified FreeGame as F
import Control.Lens 
import Control.Monad
import Data.Maybe (fromJust)

stateChange :: CharaState -> CharaAction -> CharaState
stateChange state act = state&acting.~act
                             &cellState.elapsedFrames.~0

stateForward state act actionEnd | elapsed > actionEnd = stateChange state Stopping
                                 | otherwise = state&cellState.elapsedFrames+~1
    where
      elapsed = state^.cellState^.elapsedFrames


instance FieldObject Character where
    actOn cmd (props, states) = 
        let moveCmd = moveCommand cmd
            action  = charaAction states
            effectCmd = effectCommand cmd
        in (props, effectAct (moveAct moveCmd action) effectCmd)
        where
          elapsed = states^.cellState^.elapsedFrames
          elapsed' = if elapsed > frameLoop then 0 else elapsed + 1
          actionElapsed = if elapsed > 32 then 0 else elapsed + 1
          stateChange' = stateChange states
          moveAct :: MoveCommand -> CharaAction -> CharaState
          moveAct Nuetral _  = states&cellState.elapsedFrames.~elapsed'
          moveAct (Move cmd_dir) Stopping = states&direct.~cmd_dir
                                                  &acting.~(Walking cmd_dir)
                                                  &cellState.elapsedFrames.~elapsed'
          moveAct (Move cmd_dir) (Walking dir) | dir == cmd_dir
            = let pos' = slideRCoord (states^.cellState^.pos) dir :: RCoord
                  (pos'', dirs') = nextDirect pos'
                  cell' = cellMoves (states^.cellState^.cell) dirs' [] :: Cell
              in states&direct.~dir
                       &acting.~(Walking dir)
                       &cellState%~(cell.~cell')
                                  .(pos.~pos'')
                                  .(elapsedFrames.~actionElapsed)
                                                | otherwise 
            =  states&acting.~Stopping
                     &cellState.elapsedFrames.~0
          moveAct (Stop dir) _ = states&acting.~Stopping
                                       &cellState.elapsedFrames.~0
                                       &direct.~dir 
          moveAct _ _ = states&acting.~Stopping
                              &cellState.elapsedFrames.~0

          effectAct state Evolve | state^.acting == Stopping = stateChange state Whirlslash 
                                 | state^.acting == Whirlslash = stateForward state Whirlslash 60 
                                 | otherwise =  stateChange state Stopping
          effectAct state ENothing = stateForward state (state^.acting) 60

    -- -- empty decl
    effect (_, state) cmd = M.insert (state^.cellState^.cell) (ActionCommand Nuetral ENothing) cmd


movePlayer :: Commands -> Character -> FieldMap -> Character
movePlayer cmd (me, state) f = actOn (fromJust (M.lookup (state^.cellState^.cell) cmd)) $ (me, state)

moveView :: FieldMap -> Character -> Coord -> Coord
moveView f (me, state) vp =
    let
        c = state^.cellState^.cell :: Cell
        p  = state^.cellState^.pos :: RCoord
        abpos = fieldPosition transMod (mapSize f) vp c p
        in (case abpos of 
             F.V2 px py | px <= (defaultWidth/3)    -> (+) $ F.V2 4 0 
                        | px >= (defaultWidth*2/3)  -> flip (-) $ F.V2 4 0
                        | py <= (defaultHeight/3)   -> (+) $ F.V2 0 4
                        | py >= (defaultHeight*2/3) -> flip (-) $ F.V2 0 4
                        | otherwise -> id
       ) vp

