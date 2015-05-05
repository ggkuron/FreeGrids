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
import qualified FRP.Elerea.Simple as E
import World.Data
import World.Field.Field
import World.Field.Entities

import World.Command
import Renderer.Renderer
import Renderer.Field
import Data.Maybe (catMaybes, fromJust, listToMaybe, maybeToList)
import qualified Data.Map as M
import Language.Haskell.TH hiding(Range)
import Paths_Grids

F.loadBitmapsWith [|getDataFileName|] "../images"

main :: IO (Maybe a)
main = F.runGame F.Windowed (F.Box (F.V2 0 0) (F.V2 defaultWidth defaultHeight)) $ do
    font <- F.embedIO $ F.loadFont "VL-Gothic-Regular.ttf"
    let ?font = font
    mainLoop me

mainLoop :: (?font:: F.Font) => (CharaProps, CharaState) -> F.Game a
mainLoop me = do
    let initial_origin = V2 0 0 :: Coord
        whole_rect = (0,0,defaultWidth, defaultHeight)
        initial_mapIndex = Cell(5,5)
        initial_map = fromJust $ M.lookup initial_mapIndex fieldMap
        initial_cmd = M.fromList [] :: Commands

    (directionKey, directionKeySink) <- F.embedIO $ E.external []

    network <- F.embedIO $ do
        E.start $ mdo
            cmd      <- E.transfer3 initial_cmd (\k p f cmd -> readCommands cmd k p f) directionKey player' field'
            player   <- E.transfer2 me (\c f p -> movePlayer c p f) cmd field' :: (E.SignalGen (E.Signal Character))
            player'  <- E.delay me player 
            field    <- E.transfer initial_map (\p f -> f) player 
            field'   <- E.delay initial_map field
            viewport <- E.transfer2 initial_origin (\p f v -> moveView f p v) player field :: (E.SignalGen (E.Signal Coord))
            return $ render whole_rect <$> player <*> viewport <*> field
    F.foreverFrame $ do
        inp <- filterM F.keyPress $ M.keys keyDirectionMap ++ [keyA, keyB]
        join $ F.embedIO $ directionKeySink inp >> network 

readCommands :: Commands -> [F.Key] -> Character -> FieldMap -> Commands
readCommands cmd keys c f =
    let dirInput = listToMaybe $ concat $ map (\k -> maybeToList $ M.lookup k keyDirectionMap) keys :: Maybe Direct
        cCell = (snd c)^.cellState^.cell :: Cell
        objCells = map ((^.cell).snd) (mapObjects f)
        mACmd = fromJust $ M.lookup cCell cmd ::ActionCommand  
        effectCmd = effectCommand mACmd
        blockedDir = adjacentDirections objCells cCell :: [Direct]
        moveCmd :: MoveCommand
        moveCmd = case dirInput of
                      Just dir | dir `elem` blockedDir -> Stop dir
                               | otherwise -> Move dir 
                      _ -> Nuetral
        -- TODO: Signalgenの状態をhandleして生成する
        effectCmd' :: EffectCommand
        effectCmd' = case keys of [] -> ENothing
                                  _ | keyA `elem` keys -> Evolve
                                    | otherwise ->  ENothing
    in M.insert cCell (ActionCommand{moveCommand = moveCmd
                                      ,effectCommand = effectCmd'}) cmd


render :: (?font :: F.Font) => Rect -> Character -> Coord -> FieldMap -> F.Frame ()
render displaySize m@(me, state) vp f = do
    renderBackGround m vp f
    renderOwn m vp f
    renderDebugInfo displaySize m vp f
    where
        renderDebugInfo :: (?font :: F.Font) => Rect -> Character -> Coord -> FieldMap -> F.Frame ()
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
        renderOwn :: Character -> Coord -> FieldMap -> F.Frame ()
        renderOwn  = clip

        renderBackGround :: (FieldMapI f, FieldMapR f) => Character -> Coord -> f -> F.Frame ()
        renderBackGround (_, ms) vp@(V2 x y) f = do
            let inx = mapIndex f
                (SizeTuple (mr, mc)) = mapSize f
                (Cell (r, c)) = ms^.cellState^.cell
                dirs = (if mr - 7 < r then [DOWN] else if r < 7 then [UP] else [])
                         ++ (if mc - 7 < c then [RIGHT] else if c < 7 then [LEFT] else [])
            forM_ dirs $
                  \d -> let i  = adjacentCell inx d
                            i' = adjacentCell' inx d
                        in do 
                           case M.lookup i fieldMap of
                             Nothing -> return ()
                             Just f' -> tileMaps f' (transMap d (mapSize f') vp) (cellLong transMod)
                           case M.lookup i' fieldMap of
                             Nothing -> return ()
                             Just f' -> tileMaps f' (transMap' d (mapSize f') vp) (cellLong transMod)
            tileMaps f vp  (cellLong transMod)
          where
            transMap = transMapBase _transMap
            transMap' = transMapBase _transMap'
            transMapBase :: (Coord -> Direct -> Coord -> Coord) -> Direct -> SizeTuple ->Coord -> Coord
            transMapBase tm dir st = tm (maxCoord $ st) dir
            _transMap :: Coord -> Direct -> Coord -> Coord
            _transMap (V2 sx sy) UP vp   = vp - V2 0 (cellStatic*sy) 
            _transMap (V2 sx sy) DOWN vp = vp + V2 0 (cellStatic*sy)
            _transMap (V2 sx sy) LEFT vp  = vp - V2 (cellStatic*sx) 0 
            _transMap (V2 sx sy) RIGHT vp = vp + V2 (cellStatic*sx) 0
            _transMap' :: Coord -> Direct -> Coord -> Coord
            _transMap' (V2 sx sy) UP vp   = vp + V2 (cellStatic*sx) (- cellStatic*sy) 
            _transMap' (V2 sx sy) DOWN vp = vp + V2 (- cellStatic*sx) (cellStatic*sy)
            _transMap' (V2 sx sy) LEFT vp  = vp + V2 (- cellStatic*sx) (- cellStatic*sy) 
            _transMap' (V2 sx sy) RIGHT vp = vp + V2 (cellStatic*sx) (cellStatic*sy)

keyDirectionMap:: M.Map F.Key Direct
keyDirectionMap = M.fromList 
                      [(F.KeyJ, DOWN)
                      ,(F.KeyK, UP)
                      ,(F.KeyH, LEFT)
                      ,(F.KeyL, RIGHT)]

keyA, keyB :: F.Key
keyA = F.KeyA
keyB = F.KeyB

-- Data

me_enty :: CharaProps
me_enty =  CharaProps (CellProps True True ) 
                      (M.fromList [(UP   , [_back0_png,_back1_png,_back2_png,_back3_png,_back4_png]),
                                   (DOWN , [_front0_png,_front1_png,_front2_png,_front3_png,_front4_png]),
                                   (LEFT , [_left0_png,_left1_png,_left2_png,_left3_png,_left4_png]),
                                   (RIGHT, [_right0_png,_right1_png,_right2_png,_right3_png,_right4_png])
                                  ]
                      )
me_initialState :: CharaState
me_initialState = CharaState 100 DOWN Stopping (CellState (Cell(5,5)) center 0) 

me = (me_enty, me_initialState)


fieldMap = M.fromList [(Cell(5,5),
                          FieldMap
                              (Cell(5,5))
                              [(CellProps True True, (CellState (Cell(1,1)) center 0))
                              ,(CellProps True True, (CellState (Cell(2,2)) center 0))
                              ]
                              []
                              (M.fromList [((SizedBlock15x15  (Cell(0,0)) ,SizedBlock15x15 (Cell(0,10)))
                                           , _maptips_ami_png)
                                          ,((SizedBlock15x15  (Cell(0,0)) ,SizedBlock15x15 (Cell(15,15)))
                                           , _maptips_grass_png)
                                          ]
                              ) 
                              (SizeTuple (15,15))
                         )
                        ,(Cell(5,6),
                          FieldMap 
                              (Cell(5,6))
                              [] 
                              []
                              (M.fromList [((SizedBlock15x15  (Cell(0,0)) ,SizedBlock15x15 (Cell(15,15)))
                                              , _maptips_ami_png)
                                            ]
                              )
                              (SizeTuple (15, 15))
                         )
                        ,(Cell(5,4),
                          FieldMap 
                              (Cell(5,6))
                              [] 
                              []
                              (M.fromList [((SizedBlock15x15  (Cell(0,0)) ,SizedBlock15x15 (Cell(15,15)))
                                              , _maptips_ami_png)
                                            ]
                              )
                              (SizeTuple (15, 15))
                         )
                        ,(Cell(6,5),
                          FieldMap 
                              (Cell(5,6))
                              [] 
                              []
                              (M.fromList [((SizedBlock15x15  (Cell(0,0)) ,SizedBlock15x15 (Cell(15,15)))
                                              , _maptips_ami_png)
                                            ]
                              )
                              (SizeTuple (15, 15))
                         )
 
                         ,(Cell(4,5),
                          FieldMap 
                              (Cell(5,6))
                              [] 
                              []
                              (M.fromList [((SizedBlock15x15  (Cell(0,0)) ,SizedBlock15x15 (Cell(15,15)))
                                              , _maptips_ami_png)
                                            ]
                              )
                              (SizeTuple (15, 15))
                         )
                          ,(Cell(4,4),
                          FieldMap 
                              (Cell(5,6))
                              [] 
                              []
                              (M.fromList [((SizedBlock15x15  (Cell(0,0)) ,SizedBlock15x15 (Cell(15,15)))
                                              , _maptips_ami_png)
                                            ]
                              )
                              (SizeTuple (15, 15))
                         )
                           ,(Cell(4,6),
                          FieldMap 
                              (Cell(5,6))
                              [] 
                              []
                              (M.fromList [((SizedBlock15x15  (Cell(0,0)) ,SizedBlock15x15 (Cell(15,15)))
                                              , _maptips_ami_png)
                                            ]
                              )
                              (SizeTuple (15, 15))
                         )
 
                      ]


