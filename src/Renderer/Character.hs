{-# LANGUAGE MultiWayIf #-}
module Renderer.Character where

import Prelude 
import World.Data
import World.Field.Field
import World.Field.Entities
import Renderer.Renderer
import qualified FreeGame as F
import qualified Data.Map as M
import Control.Monad
import Control.Lens
import Data.Maybe (fromJust)

instance CellEntityRender CellTip where
    cellBMP t = case t^.props^.tipType of
                    TIP_AMI   -> _maptips_ami_png
                    TIP_Grass -> _maptips_grass_png

instance CellEntityRender Character where
        cellBMP c = let elapsed = c^.charaState^.cellState^.actionStep
                        state = c^.charaState^.acting
                        dir = c^.charaState^.direct
                     in case c^.charaProps^.cellProps^.tipType of
                                  TIP_Player -> playerR state elapsed dir

playerR Walking elapsed UP | elapsed < 15 = _back1_png
                           | elapsed < 30 = _back2_png
                           | elapsed < 45 = _back3_png
                           | otherwise = _back4_png
playerR Walking elapsed DOWN | elapsed < 15 = _front1_png
                             | elapsed < 30 = _front2_png
                             | elapsed < 45 = _front3_png
                             | otherwise = _front4_png
playerR Walking elapsed LEFT| elapsed < 15 = _left1_png
                            | elapsed < 30 = _left2_png
                            | elapsed < 45 = _left3_png
                            | otherwise = _left4_png
playerR Walking elapsed RIGHT | elapsed < 15 = _right1_png
                              | elapsed < 30 = _right2_png
                              | elapsed < 45 = _right3_png
                              | otherwise = _right4_png
playerR _ _ UP = _back0_png
playerR _ _ DOWN = _front0_png
playerR _ _ LEFT = _left0_png
playerR _ _ RIGHT = _right0_png
