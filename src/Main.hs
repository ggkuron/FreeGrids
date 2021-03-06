{-# LANGUAGE ImplicitParams, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE InstanceSigs #-}

module Main where
import qualified FreeGame as F
import Prelude 
import Control.Monad hiding(const)
import Control.Applicative((<*>), (<$>))
import qualified FRP.Elerea.Simple as E
import World.Data
import World.Field.Field
import World.Field.Entities
import World.Parser
import World.Command
import Renderer.Renderer
import Renderer.Field
import Data.Maybe (fromJust, listToMaybe, maybeToList, fromMaybe)
import qualified Data.Map as M
import Language.Haskell.TH hiding(Range)
import Paths_grids

import qualified Data.Strict.Tuple as ST
import Control.Parallel.Strategies

import Debug.Trace


main :: IO (Maybe a)
main = F.runGame F.Windowed (F.Box (F.V2 0 0) (F.V2 defaultWidth defaultHeight)) $ do
    font <- F.embedIO $ F.loadFont "VL-Gothic-Regular.ttf"
    let ?font = font
    mainLoop 

mainLoop :: (?font:: F.Font) =>  F.Game a
mainLoop = do
    let initial_origin = (V2 0 0)  :: Coord
        wholeRect = (0,0,defaultWidth, defaultHeight)
        meInitialIndex = mapCell (1:!:1) (5:!:5) :: MapCell
        me = Player $ Character meEnty (meInitialState meInitialIndex) :: Player
        initialCmd = M.fromList [] :: Commands

    initialFieldData <- F.embedIO $ retrieveFieldData meInitialIndex 
    (directionKey, directionKeySink) <- F.embedIO $ E.external []

    network <- F.embedIO $ do
        E.start $ mdo
            cmd      <- E.transfer3 initialCmd readCommands directionKey player' field'
            player   <- E.transfer2 me (\c f p -> p&playerChara.~(movePlayer c f (p^.playerChara))) cmd field' :: (E.SignalGen (E.Signal Player))
            player'  <- E.delay me player :: (E.SignalGen (E.Signal Player))
            field    <- E.transfer initialFieldData (mapChanger initialFieldData) player :: (E.SignalGen (E.Signal FieldData))
            field'   <- E.delay initialFieldData field :: (E.SignalGen (E.Signal FieldData))
            viewport <- E.transfer2 initial_origin moveView player field :: (E.SignalGen (E.Signal Coord))
            return $ render wholeRect <$> player <*> viewport <*> field
    F.foreverFrame $ do
        F.setFPS 60
        inp <- filterM F.keyPress $ M.keys keyDirectionMap ++ [keyA, keyB]
        join $ F.embedIO $ directionKeySink inp >> network 

readCommands :: [F.Key] -> Player -> FieldData -> Commands -> Commands
readCommands keys p f cmd =
    let dirInput = listToMaybe $
                      concat $ map (\k -> maybeToList $ M.lookup k keyDirectionMap) keys :: Maybe Direct
        inx = p^.playerChara^.charaState^.cellState^.cell :: WorldCell
        blocks = map from $ filter (\t-> t^.props^.block) (f^.fieldTips)
        mACmd = fromJust $ M.lookup inx cmd ::ActionCommand  
        effectCmd = effectCommand mACmd
        blockedDir = adjacentDirections blocks (from . fieldCell $ inx) :: [Direct]
        moveCmd :: MoveCommand
        moveCmd = case dirInput of
                      Just dir | dir `elem` blockedDir -> Stop dir
                               | otherwise -> Move dir 
                      _ -> Nuetral
        effectCmd' :: EffectCommand
        effectCmd' = case keys of [] -> ENothing
                                  _ | keyA `elem` keys -> Evolve
                                    | otherwise ->  ENothing
     in M.insert inx (ActionCommand{moveCommand = moveCmd
                                   ,effectCommand = effectCmd'}) cmd


render :: (?font :: F.Font) => Rect -> Player -> Coord -> FieldData -> F.Frame ()
render displaySize me vp f = do
    renderBackGround me vp f
    renderOwn me vp f
    renderDebugInfo displaySize me vp f
    where
        renderDebugInfo :: (?font :: F.Font) => Rect -> Player -> Coord -> FieldData -> F.Frame ()
        renderDebugInfo displaySize me vp f = do
            let state = me^.playerChara^.charaState
                fc = state^.cellState^.cell
                fobj_ary = f^.fieldTips
                fchars = f^.fieldCharacters
            F.color F.red 
                $ F.translate (F.V2 40 40) 
                $ F.text ?font 40 $ "vp: " ++ (show vp)
            F.color F.blue
                $ F.translate (F.V2 40 80) 
                $ F.text ?font 40 $ "origin: " ++  (show cornerPointOrigin)
            F.color F.gray
                $ F.translate (F.V2 40 400) 
                $ F.text ?font 40 $ "charaPos: " ++  (show $ fieldPosition vp fc (state^.cellState^.pos))
            F.color F.yellow
                $ F.translate (F.V2 40 180) 
                $ F.text ?font 40 $ show $ state^.acting
            F.color F.yellow
                $ F.translate (F.V2 (50) 100) 
                $ F.text ?font 20 $ show $ state^.cellState^.cell
        renderOwn :: Player -> Coord -> FieldData -> F.Frame ()
        renderOwn  p = clip $ p^.playerChara 

        renderBackGround ::  Player -> Coord -> FieldData -> F.Frame ()
        renderBackGround c vp f = tileMaps f vp 

keyDirectionMap:: M.Map F.Key Direct
keyDirectionMap = M.fromList 
                      [(F.KeyJ, DOWN)
                      ,(F.KeyK, UP)
                      ,(F.KeyH, LEFT)
                      ,(F.KeyL, RIGHT)]

keyA, keyB :: F.Key
keyA = F.KeyA
keyB = F.KeyB


mapChanger :: FieldData -> Player -> FieldData -> FieldData
mapChanger i c f = aroundField (c^.playerChara^.charaState^.cellState^.cell) (worldCell(15:!:18)) i
               

retrieveFieldData :: WorldIndex w => w -> IO FieldData
retrieveFieldData wi = do
        filepath <- getDataFileName "static/map/field.dat"
        fieldMap filepath

meEnty :: CharaProps
meEnty =  CharaProps (CellProps True TIP_Player) 
                      

meInitialState :: WorldIndex w => w -> CharaState
meInitialState wi = CharaState 100 DOWN Stopping (CellState (fieldCell wi) center 0) 


createMap :: [(BCell, [(Cell, Cell, TipType, Bool)], [Character])] -> FieldData
createMap props = FieldData (concat $ map (\(bc,cp,_) -> concat $ map (\(s,e,t,b) -> tipList [MapCell bc a | a <- [(ACell s) .. (ACell e)]] $ CellProps b t) cp) props)
                            (concat $ map (\(_,_,c) -> c) props)

-- fieldMap :: FieldData
fieldMap filepath = do
        tips <- parseFieldData filepath
        return $ FieldData (join.join $ fromJust tips) []
           

