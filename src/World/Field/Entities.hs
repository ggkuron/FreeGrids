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

stateChange :: CharaState -> CharaAction -> CharaState
stateChange state act = state&acting.~act
                             &cellState.elapsedFrames.~0

stateForward state act actionEnd | (state^.cellState^.elapsedFrames) > actionEnd = stateChange state Stopping
                                 | otherwise = state&cellState.elapsedFrames+~1

nextDirect :: RCoord -> (RCoord, [Direct])
nextDirect (rcx :!: rcy) = 
    let slideUpdate s up down | isSlideUpped s  = (slideNegative, [up])
                              | isSlideDowned s = (slidePositive, [down])
                              | otherwise = (s, [])
        (rcx', xdir) = slideUpdate rcx RIGHT LEFT
        (rcy', ydir) = slideUpdate rcy DOWN UP
    in ((rcx' :!: rcy'), xdir ++ ydir)

instance FieldActor Character where
    actOn fs f cmd (props, states) = 
        let moveCmd = moveCommand cmd
            action  = charaAction states
            effectCmd = effectCommand cmd
        in (props, effectAct (moveAct fs f moveCmd action) effectCmd)
        where
          elapsed = states^.cellState^.elapsedFrames
          elapsed' = if elapsed > frameLoop then 0 else elapsed + 1
          actionElapsed = if elapsed > 32 then 0 else elapsed + 1
          stateChange' = stateChange states
          moveAct :: (FieldMapI f, FieldSheet fs) => fs ->  f ->  MoveCommand -> CharaAction -> CharaState
          moveAct _ f Nuetral _  = states&cellState.elapsedFrames.~elapsed'
          moveAct _ f (Move cmd_dir) Stopping = states&direct.~cmd_dir
                                                  &acting.~(Walking cmd_dir)
                                                  &cellState.elapsedFrames.~elapsed'
          moveAct fs f (Move cmd_dir) (Walking dir) 
            | dir == cmd_dir
            = let c    = states^.cellState^.fieldCell
                  mc   = states^.cellState^.mapCell
                  pos' = slideRCoord (states^.cellState^.pos) dir :: RCoord
                  (pos'', dirs') = nextDirect $ pos'
                  fc' = fmap (cellMoves dirs' []) c :: FieldCell
                  fouterdir = cellOuterDirection (mapSize f) fc'
                  (mc', fc'') = case fouterdir of
                                (d:ds) | dir == d ->  (fmap (adjacentCell d) mc, outerEdgeCell fs mc d fc')
                                       | otherwise -> (mc, fc')
                                _ -> (mc, fc')
              in states&direct.~dir
                       &acting.~(Walking dir)
                       &cellState%~(fieldCell.~fc'')
                                  .(mapCell.~mc')
                                  .(pos.~pos'')
                                  .(elapsedFrames.~actionElapsed)
            | otherwise 
            =  states&acting.~Stopping
                     &cellState.elapsedFrames.~0
          moveAct _ f (Stop dir) _ = states&acting.~Stopping
                                       &cellState.elapsedFrames.~0
                                       &direct.~dir 
          moveAct _ f _ _ = states&acting.~Stopping
                              &cellState.elapsedFrames.~0

          effectAct state Evolve | state^.acting == Stopping = stateChange state Whirlslash 
                                 | state^.acting == Whirlslash = stateForward state Whirlslash 60 
                                 | otherwise =  stateChange state Stopping
          effectAct state ENothing = stateForward state (state^.acting) 60

    -- -- empty decl
    effect (_, state) cmd = M.insert (cellIndex $ state^.cellState) (ActionCommand Nuetral ENothing) cmd


movePlayer :: FieldSheet s => Commands -> s -> FieldMap -> Character -> Character
movePlayer cmds s f me@(_, state) = actOn s f cmd me
    where cmd = fromJust (M.lookup (cellIndex $ state^.cellState) cmds)

viewEdgeV = defaultHeight / 3
viewEdgeH = defaultWidth / 3

charaPos vp f state = fieldPosition vp (mapSize f) (state^.mapCell) (state^.fieldCell) (state^.pos)

moveView :: Character -> FieldMap -> Coord -> Coord
moveView (me, state) f vp =
    let (V2 sx sy) = fieldSizeTrans vp
        moveStep = 4
    in (case charaPos vp f (state^.cellState)  of 
             F.V2 px py | px <= viewEdgeH -> (+) $ F.V2 moveStep 0 
                        | px >= (defaultWidth - viewEdgeH) -> flip (-) $ F.V2 moveStep 0
                        | py <= viewEdgeV -> (+) $ F.V2 0 moveStep 
                        | py >= (defaultHeight - viewEdgeV) -> flip (-) $ F.V2 0 moveStep
                        | otherwise -> id
       ) vp


