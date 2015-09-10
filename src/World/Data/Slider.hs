{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module World.Data.Slider 
( module World.Data.Slider
, module Control.Lens 
) where

import Control.Lens hiding(to, from)
import Prelude hiding(max, min)

data Bound a = Bound (a, a) a
    deriving (Show,Eq)
type Slider = Bound Int

slider :: Int -> Slider
slider per = Bound (-100, 100) per

instance Bounded Slider where
    minBound = slider $ -100
    maxBound = slider 100

bmin, bmax, bval :: forall t. Bound t -> t
bmin (Bound (min, _) _) = min
bmax (Bound (_, max) _) = max
bval (Bound (_, _) val) = val


slideUp :: (Int -> Int) -> Slider -> Slider
slideUp f b | isSlideUpped b = slider 100
            | otherwise  = slider $ f.bval $ b

slideDown :: (Int -> Int) -> Slider -> Slider
slideDown f b | isSlideDowned b = slider $ -100
              | otherwise  = slider $ f.bval $ b

isSlideUpped :: Slider -> Bool
isSlideUpped (Bound (_, max) val) = max <= val

isSlideDowned :: Slider -> Bool
isSlideDowned (Bound (min, _) val) = min >= val

isSlideMax :: Slider -> Bool
isSlideMax s = or $ map (\f -> f s) [isSlideUpped, isSlideDowned] 

