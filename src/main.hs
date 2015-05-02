{-# LANGUAGE ImplicitParams, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RecursiveDo #-}

module Main where
import qualified FreeGame as F
import Prelude 
import Control.Monad hiding(const)
import Control.Applicative((<*>), (<$>))
import Control.Lens 
import qualified FRP.Elerea.Simple as E
import World.Data
import World.Field.Field
import World.Field.Entities

import World.Command
import Renderer.Renderer
import Renderer.Field
import Data.Maybe (catMaybes, fromJust, listToMaybe, maybeToList)
import qualified Data.Map as Map
import Language.Haskell.TH hiding(Range)
import Paths_Grids
-- debug
-- import System.IO(putStrLn)
--


F.loadBitmapsWith [|getDataFileName|] "../images"

me_enty :: CharaProps
me_enty =  CharaProps (CellProps True True ) 
                      (Map.fromList [(UP   , [_back0_png,_back1_png,_back2_png,_back3_png,_back4_png]),
                                     (DOWN , [_front0_png,_front1_png,_front2_png,_front3_png,_front4_png]),
                                     (LEFT , [_left0_png,_left1_png,_left2_png,_left3_png,_left4_png]),
                                     (RIGHT, [_right0_png,_right1_png,_right2_png,_right3_png,_right4_png])
                                    ]
                      )
me_initialState :: CharaState
me_initialState = CharaState 100 DOWN Stopping (CellState (Cell(5,5)) center 0) 

me = (me_enty, me_initialState)


fieldMap = Map.fromList [(Cell(5,5),
                          FieldMap
                              (Cell(5,5))
                              [(CellProps True True, (CellState (Cell(1,1)) center 0))
                              ,(CellProps True True, (CellState (Cell(2,2)) center 0))
                              ]
                              []
                              (Map.fromList [((SizedBlock25x25  (Cell(0,0)) ,SizedBlock25x25 (Cell(0,10)))
                                             , _maptips_ami_png)
                                            ,((SizedBlock25x25  (Cell(5,0)) ,SizedBlock25x25 (Cell(25,25)))
                                             , _maptips_grass_png)
                                            ]
                              ) 
                              (SizeTuple (25,25))
                         )
                        ,(Cell(5,6),
                          FieldMap 
                              (Cell(5,6))
                              [] 
                              []
                              (Map.fromList [((SizedBlock25x25  (Cell(0,0)) ,SizedBlock25x25 (Cell(20,25)))
                                              , _maptips_ami_png)
                                            ]
                              )
                              (SizeTuple (25, 25))
                         )
                      ]

main :: IO (Maybe a)
main = F.runGame F.Windowed (F.Box (F.V2 0 0) (F.V2 defaultWidth defaultHeight)) $ do
    font <- F.embedIO $ F.loadFont "VL-Gothic-Regular.ttf"
    let ?font = font
    mainLoop me

mainLoop :: (?font:: F.Font) => (CharaProps, CharaState) -> F.Game a
mainLoop me = do
    let initial_origin = F.V2 0 0
        whole_rect = (0,0,defaultWidth, defaultHeight)
        initial_mapIndex = Cell(5,5)
        initial_map = fromJust $ Map.lookup initial_mapIndex fieldMap
        initial_cmd = Map.fromList [] :: Commands

    (directionKey, directionKeySink) <- F.embedIO $ E.external []

    network <- F.embedIO $ do
        E.start $ mdo
            cmd      <- E.transfer3 initial_cmd (\k p f cmd -> readCommands cmd k p f) directionKey player' field'
            player   <- E.transfer2 me (\c f p -> movePlayer c p f) cmd field' :: (E.SignalGen (E.Signal Character))
            player'  <- E.delay me player 
            field    <- E.transfer initial_map (\p f -> f) player 
            field'   <- E.delay initial_map field
            viewport <- E.transfer2 initial_origin (\p f v -> moveView f p v) player field :: (E.SignalGen (E.Signal F.Vec2))
            return $ render whole_rect <$> player <*> viewport <*> field
    F.foreverFrame $ do
        inp <- filterM F.keyPress $ Map.keys keyDirectionMap ++ [keyA, keyB]
        join $ F.embedIO $ directionKeySink inp >> network 

readCommands :: Commands -> [F.Key] -> Character -> FieldMap -> Commands
readCommands cmd keys c f =
    let dirInput = listToMaybe $ concat $ map (\k -> maybeToList $ Map.lookup k keyDirectionMap) keys :: Maybe Direct
        cCell = (snd c)^.cellState^.cell :: Cell
        objCells = map ((^.cell).snd) (mapObjects f)
        mACmd = fromJust $ Map.lookup cCell cmd ::ActionCommand  
        effectCmd = effectCommand mACmd
        blockedDir = adjacentDirections objCells cCell :: [Direct]
        moveCmd :: MoveCommand
        moveCmd = case dirInput of
                      Just dir | dir `elem` blockedDir -> Stop dir
                               | otherwise -> Move dir 
                      _ -> Nuetral
        -- TODO: Singlanの状態をhandleして生成する
        effectCmd' :: EffectCommand
        effectCmd' = case keys of [] -> ENothing
                                  _ | keyA `elem` keys -> Evolve
                                    | otherwise ->  ENothing
    in Map.insert cCell (ActionCommand{moveCommand = moveCmd
                                      ,effectCommand = effectCmd'}) cmd

render :: (?font :: F.Font) => Rect -> Character -> F.Vec2 -> FieldMap -> F.Frame ()
render displaySize m@(me, state) vp f = do
    renderBackGround vp f
    renderOwn m vp f
    renderDebugInfo displaySize m vp f
    where
        renderDebugInfo :: (?font :: F.Font) => Rect -> Character -> F.Vec2 -> FieldMap -> F.Frame ()
        renderDebugInfo displaySize (me, state) vp f = do
            let c = state^.cellState^.cell
                fobj_ary = mapObjects f
                fchars = mapCharacters f
                msize = mapSize f
            F.color F.yellow 
                $ F.thickness 3 
                $ renderGrids displaySize cellStatic
            F.color F.black 
                $ F.translate (F.V2 150 150) 
                $ F.text ?font 30 $ show $ state^.acting
            F.color F.black 
                $ F.translate (F.V2 40 40) 
                $ F.text ?font 40 $ show $ state^.cellState^.elapsedFrames
        renderOwn :: Character -> F.Vec2 -> FieldMap -> F.Frame ()
        renderOwn  = clip

        renderBackGround :: (FieldMapI f, FieldMapR f) => F.Vec2 -> f -> F.Frame ()
        renderBackGround vp f = do
            let inx = mapIndex f
            forM_ allDirection $
                  \d -> let i = adjacentCell inx d
                        in  case Map.lookup i fieldMap of
                                Nothing -> return ()
                                Just f' -> tileMaps f' (transMap d (mapSize f') vp) $ normalTrans transMod
            tileMaps f vp $ normalTrans transMod
          where
            _transMap ::  Double -> Direct -> F.Vec2 -> F.Vec2
            _transMap y UP    = (-) $ F.V2 0 (cellStatic*y)
            _transMap y DOWN  = (+) $ F.V2 0 (cellStatic*y)
            _transMap x LEFT  = (-) $ F.V2 (cellStatic*x) 0
            _transMap x RIGHT = (+) $ F.V2 (cellStatic*x) 0
            transMap :: Direct -> SizeTuple -> F.Vec2 -> F.Vec2
            transMap dir st = let F.V2 sx sy = coordVec.maxCoord $ st 
                              in case dir of
                                 UP    -> _transMap sy dir
                                 DOWN  -> _transMap sy dir
                                 LEFT  -> _transMap sx dir
                                 RIGHT -> _transMap sx dir

keyDirectionMap:: Map.Map F.Key Direct
keyDirectionMap = Map.fromList 
                      [(F.KeyJ, DOWN)
                      ,(F.KeyK, UP)
                      ,(F.KeyH, LEFT)
                      ,(F.KeyL, RIGHT)]

keyA, keyB :: F.Key
keyA = F.KeyA
keyB = F.KeyB

-- 
movePlayer :: Commands -> Character -> FieldMap -> Character
movePlayer cmd (me, state) f = actOn (fromJust (Map.lookup (state^.cellState^.cell) cmd)) $ (me, state)

moveView :: FieldMap -> Character -> F.Vec2 -> F.Vec2
moveView f (me, state) vp =
    let
        c = state^.cellState^.cell :: Cell
        p  = state^.cellState^.pos :: RCoord
        abpos = picPos transMod (mapSize f) vp c p
        in (case abpos of 
             F.V2 px py | px <= (defaultWidth/3)    -> (+) $ F.V2 4 0 
                        | px >= (defaultWidth*2/3)  -> flip (-) $ F.V2 4 0
                        | py <= (defaultHeight/3)   -> (+) $ F.V2 0 4
                        | py >= (defaultHeight*2/3) -> flip (-) $ F.V2 0 4
                        | otherwise -> id
       ) vp

