{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
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

import Debug.Trace

tipList :: WorldIndex w => [w] -> CellProps -> [CellTip]
tipList r p = [CellTip p $ CellState (fieldCell c) center 0 | c <- r]

fieldPosition :: Coord -> WorldCell -> RCoord -> Coord
fieldPosition vp c crd = addRCoord (cornerPoint' vp c) crd

cellPosition :: Coord -> WorldCell -> RCoord -> Coord
cellPosition vp c crd = addRCoord (cornerPoint' vp c) crd


stateChange :: CharaState -> CharaAction -> CharaState
stateChange cs act  = cs&acting.~act
                        &cellState.actionStep.~0

stateForward state act actionEnd | (state^.cellState^.actionStep) > actionEnd = stateChange state Stopping
                                 | otherwise = state&cellState.actionStep+~1

nextDirect :: RCoord -> (RCoord, [Direct])
nextDirect (rcx :!: rcy) = 
    let slideUpdate s up down | isSlideUpped s  = (slideNegative, [up])
                              | isSlideDowned s = (slidePositive, [down])
                              | otherwise = (s, [])
        (rcx', xdir) = slideUpdate rcx RIGHT LEFT
        (rcy', ydir) = slideUpdate rcy DOWN UP
    in ((rcx' :!: rcy'), xdir ++ ydir)

instance FieldActor Character where
    actOn f cmd c = 
        let moveCmd = moveCommand cmd
            effectCmd = effectCommand cmd
            action  = c^.charaState^.acting
        in c&charaState.~(effectAct (moveAct f moveCmd action) effectCmd)
        where
          elapsed = c^.charaState^.cellState^.actionStep
          elapsed' = if elapsed > frameLoop then 0 else elapsed + 1
          actionElapsed = if elapsed > 32 then 0 else elapsed + 1
          stateChange' = stateChange $ c^.charaState
          moveAct :: FieldData ->  MoveCommand -> CharaAction -> CharaState
          moveAct f Nuetral _ = c^.charaState&cellState.actionStep.~elapsed'
          moveAct f (Move cmd_dir) Stopping =  c^.charaState&direct.~cmd_dir
                                                            &acting.~Walking 
                                                            &cellState.actionStep.~0
          moveAct f (Move cmdDir) Walking 
            | c^.charaState^.direct == cmdDir
            = let cl  = c^.charaState^.cellState^.cell :: WorldCell
                  rc  = c^.charaState^.cellState^.pos :: RCoord
                  rc' = slideRCoord rc  (c^.charaState^.direct) :: RCoord
                  (rc'', dirs') = nextDirect rc'
                  fc = wrap (cellMoves dirs' []) cl
                  fc' =  if blockCheck f fc then cl else fc
              in c^.charaState&acting.~(Walking)
                              &cellState%~(cell.~fc')
                                         .(pos.~rc'')
                                         .(actionStep.~actionElapsed)
            | otherwise 
            =  c^.charaState&acting.~Stopping
                            &cellState.actionStep.~0
          moveAct f (Stop dir) _ = c^.charaState&acting.~Stopping
                                                &cellState.actionStep.~actionElapsed
                                                &direct.~dir 
          moveAct f _ _ = c^.charaState&acting.~Stopping
                                       &cellState.actionStep.~0

          effectAct state Evolve | state^.acting == Stopping = stateChange state Whirlslash 
                                 | state^.acting == Whirlslash = stateForward state Whirlslash 60 
                                 | otherwise =  stateChange state Stopping
          effectAct state ENothing = stateForward state (state^.acting) 60

    -- -- empty decl
    effect c cmd = M.insert (c^.charaState^.cellState^.cell) (ActionCommand Nuetral ENothing) cmd


movePlayer :: Commands -> FieldData ->  Character -> Character
movePlayer cmds f me = actOn f cmd me
    where cmd = fromJust (M.lookup (me^.charaState^.cellState^.cell) cmds)

viewEdgeV = defaultHeight / 3
viewEdgeH = defaultWidth / 3

charaPos :: Coord -> Character -> Coord
charaPos vp c = fieldPosition vp (c^.charaState^.cellState^.cell) (c^.charaState^.cellState^.pos)

moveView :: Player -> FieldData -> Coord -> Coord
moveView c f vp =
    let (V2 sx sy) = fieldSizeTrans vp
        moveStep = 4
    in (case charaPos vp (c^.playerChara)  of 
             F.V2 px py | px <= viewEdgeH -> (+) $ F.V2 moveStep 0 
                        | px >= (defaultWidth - viewEdgeH) -> flip (-) $ F.V2 moveStep 0
                        | py <= viewEdgeV -> (+) $ F.V2 0 moveStep 
                        | py >= (defaultHeight - viewEdgeV) -> flip (-) $ F.V2 0 moveStep
                        | otherwise -> id
       ) vp


