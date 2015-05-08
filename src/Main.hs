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

import World.Command
import Renderer.Renderer
import Renderer.Field
import Data.Maybe (catMaybes, fromJust, listToMaybe, maybeToList)
import qualified Data.Map as M
import Language.Haskell.TH hiding(Range)
import Paths_Grids

import Debug.Trace

F.loadBitmapsWith [|getDataFileName|] "../images"

main :: IO (Maybe a)
main = F.runGame F.Windowed (F.Box (F.V2 0 0) (F.V2 defaultWidth defaultHeight)) $ do
    font <- F.embedIO $ F.loadFont "VL-Gothic-Regular.ttf"
    let ?font = font
    mainLoop 

mainLoop :: (?font:: F.Font) =>  F.Game a
mainLoop = do
    let initial_origin = V2 0 0 :: Coord
        whole_rect = (0,0,defaultWidth, defaultHeight)
        initialMapIndex = mcell (2,2)
        me' = me initialMapIndex
        initialMap = fromJust $ lookupFieldMap fieldSheet initialMapIndex
        initialCmd = M.fromList [] :: Commands

    (directionKey, directionKeySink) <- F.embedIO $ E.external []

    network <- F.embedIO $ do
        E.start $ mdo
            cmd      <- E.transfer3 initialCmd readCommands directionKey player' field'
            player   <- E.transfer2 me' (\c f p -> movePlayer c fieldSheet f p) cmd field' :: (E.SignalGen (E.Signal Character))
            player'  <- E.delay me' player 
            field    <- E.transfer initialMap mapChanger player 
            field'   <- E.delay initialMap field
            viewport <- E.transfer2 initial_origin moveView player field :: (E.SignalGen (E.Signal Coord))
            return $ render whole_rect <$> player <*> viewport <*> field
    F.foreverFrame $ do
        inp <- filterM F.keyPress $ M.keys keyDirectionMap ++ [keyA, keyB]
        join $ F.embedIO $ directionKeySink inp >> network 

readCommands :: [F.Key] -> Character -> FieldMap -> Commands -> Commands
readCommands keys c f cmd =
    let dirInput = listToMaybe $
                      concat $ map (\k -> maybeToList $ M.lookup k keyDirectionMap) keys :: Maybe Direct
        mCell = (snd c)^.cellState^.mapCell :: MapCell
        cCell = (snd c)^.cellState^.fieldCell :: FieldCell
        objCells = map ((^.fieldCell).snd) (mapObjects f) :: [FieldCell]
        mACmd = fromJust $ M.lookup (mCell, cCell) cmd ::ActionCommand  
        effectCmd = effectCommand mACmd
        blockedDir = adjacentFieldDirections objCells cCell :: [Direct]
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
    in M.insert (mCell,cCell) (ActionCommand{moveCommand = moveCmd
                                            ,effectCommand = effectCmd'}) cmd


render :: (?font :: F.Font) => Rect -> Character -> Coord -> FieldMap -> F.Frame ()
render displaySize m@(me, state) vp f = do
    renderBackGround m vp f
    renderOwn m vp f
    renderDebugInfo displaySize m vp f
    where
        renderDebugInfo :: (?font :: F.Font) => Rect -> Character -> Coord -> FieldMap -> F.Frame ()
        renderDebugInfo displaySize me@(props, state) vp f = do
            let fc = state^.cellState^.fieldCell
                fobj_ary = mapObjects f
                fchars = mapCharacters f
            F.color F.red 
                $ F.translate (F.V2 40 40) 
                $ F.text ?font 40 $ "vp: " ++ (show vp)
            F.color F.blue
                $ F.translate (F.V2 40 80) 
                $ F.text ?font 40 $ "origin: " ++  (show cornerPointOrigin)
            F.color F.gray
                $ F.translate (F.V2 40 400) 
                $ F.text ?font 40 $ "charaPos: " ++  (show $ charaPos vp f (state^.cellState))
            F.color F.yellow
                $ F.translate (F.V2 40 180) 
                $ F.text ?font 40 $ show $ state^.cellState^.fieldCell
            F.color F.yellow
                $ F.translate (F.V2 40 100) 
                $ F.text ?font 40 $ show $ state^.cellState^.mapCell

        renderOwn :: Character -> Coord -> FieldMap -> F.Frame ()
        renderOwn  = clip

        renderBackGround :: (FieldMapI f, FieldMapR f) => Character -> Coord -> f -> F.Frame ()
        renderBackGround (_, ms) vp f = do
            let inx = mapIndex f
                (SizeTuple (mr, mc)) = mapSize f
                (FieldObject (Cell (r, c))) = ms^.cellState^.fieldCell
                dirs = (if mr - 7 < r then [DOWN] else if r < 7 then [UP] else [])
                         ++ (if mc - 7 < c then [RIGHT] else if c < 7 then [LEFT] else [])
            forM_ dirs $
                  \d -> let i  = fmap (adjacentCell d) inx 
                            i' = fmap (adjacentCell' d) inx 
                        in do 
                           case lookupFieldMap fieldSheet i of
                             Just f' -> tileMaps f' vp (cellLong transMod)
                             _ -> return ()
                           case lookupFieldMap fieldSheet i' of
                             Just f' -> tileMaps f' vp (cellLong transMod)
                             _ -> return ()
            tileMaps f vp (cellLong transMod)

keyDirectionMap:: M.Map F.Key Direct
keyDirectionMap = M.fromList 
                      [(F.KeyJ, DOWN)
                      ,(F.KeyK, UP)
                      ,(F.KeyH, LEFT)
                      ,(F.KeyL, RIGHT)]

keyA, keyB :: F.Key
keyA = F.KeyA
keyB = F.KeyB


mapChanger :: Character -> FieldMap -> FieldMap
mapChanger c f = let minx  = (snd c)^.cellState^.mapCell
                     next  = lookupFieldMap fieldSheet minx
                  in case next of
                        Just n -> n
                        _ -> f

-- Data

meEnty :: CharaProps
meEnty =  CharaProps (CellProps True True) 
                      (M.fromList [(UP   , [_back0_png,_back1_png,_back2_png,_back3_png,_back4_png]),
                                   (DOWN , [_front0_png,_front1_png,_front2_png,_front3_png,_front4_png]),
                                   (LEFT , [_left0_png,_left1_png,_left2_png,_left3_png,_left4_png]),
                                   (RIGHT, [_right0_png,_right1_png,_right2_png,_right3_png,_right4_png])
                                  ]
                      )
meInitialState :: MapCell -> CharaState
meInitialState mc = CharaState 100 DOWN Stopping (CellState mc (fcell(5,5)) center 0) 

me mc = (meEnty, meInitialState mc)


data FieldS = FieldS { innerFieldHash :: M.Map MapCell FieldMap }

fieldSheet = FieldS fieldMap 

instance FieldSheet FieldS where
    outerEdgeCell :: FieldS -> MapCell ->  Direct ->  FieldCell -> FieldCell
    outerEdgeCell fs mc d init =  case lookupFieldMap fs $ fmap (adjacentCell d) mc of
                              Just f' -> let rd = turnBack d
                                             ms = mapSize f'
                                             in fmap (\fc -> Cell $ edgeTuple ms rd fc) init
                              _ -> init

    lookupFieldMap :: FieldS -> MapCell -> Maybe FieldMap
    lookupFieldMap f mc = M.lookup mc (innerFieldHash f)


fieldMap = M.fromList [(mcell(2,2),
                          FieldMap (mcell(2,2))
                              [(CellProps True True, (CellState (mcell(5,5)) (fcell(1,1)) center 0))
                              ,(CellProps True True, (CellState (mcell(5,5)) (fcell(2,2)) center 0))
                              ]
                              []
                              (M.fromList [((SizedBlock15x15  (fcell(0,0)) ,SizedBlock15x15 (fcell(0,10)))
                                           , _maptips_ami_png)
                                          ,((SizedBlock15x15  (fcell(0,0)) ,SizedBlock15x15 (fcell(15,15)))
                                           , _maptips_grass_png)
                                          ]
                              ) 
                              (SizeTuple (15,15))
                         )
                        ,(mcell(1,2),
                          FieldMap 
                              (mcell(1,2))
                              [] 
                              []
                              (M.fromList [((SizedBlock15x15  (fcell(0,0)) ,SizedBlock15x15 (fcell(15,15)))
                                              , _maptips_ami_png)
                                            ]
                              )
                              (SizeTuple (15, 15))
                         )
                        ,(mcell(2,1),
                          FieldMap 
                              (mcell(2,1))
                              [] 
                              []
                              (M.fromList [((SizedBlock15x15  (fcell(0,0)) ,SizedBlock15x15 (fcell(15,15)))
                                              , _maptips_ami_png)
                                            ]
                              )
                              (SizeTuple (15, 15))
                         )
                        ,(mcell(6,5),
                          FieldMap 
                              (mcell(5,6))
                              [] 
                              []
                              (M.fromList [((SizedBlock15x15  (fcell(0,0)) ,SizedBlock15x15 (fcell(15,15)))
                                              , _maptips_ami_png)
                                            ]
                              )
                              (SizeTuple (15, 15))
                         )
                         ,(mcell(1,1),
                          FieldMap 
                              (mcell(1,1))
                              [(CellProps True True, (CellState (mcell(5,5)) (fcell(1,1)) center 0))] 
                              []
                              (M.fromList [((SizedBlock15x15  (fcell(0,0)) ,SizedBlock15x15 (fcell(15,15)))
                                              , _maptips_ami_png)
                                            ]
                              )
                              (SizeTuple (15, 15))
                         )
                          ,(mcell(1,3),
                          FieldMap 
                              (mcell(1,3))
                              [] 
                              []
                              (M.fromList [((SizedBlock15x15  (fcell(0,0)) ,SizedBlock15x15 (fcell(15,15)))
                                              , _maptips_ami_png)
                                            ]
                              )
                              (SizeTuple (15, 15))
                         )
                           ,(mcell(2,3),
                          FieldMap 
                              (mcell(2,3))
                              [] 
                              []
                              (M.fromList [((SizedBlock15x15  (fcell(0,0)) ,SizedBlock15x15 (fcell(15,15)))
                                              , _maptips_ami_png)
                                            ]
                              )
                              (SizeTuple (15, 15))
                         )
 
                      ]


