{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module World.Data.Slider 
( module World.Data.Slider
, module Control.Lens
) where

import Data.Ix
import Control.Lens 

-- (Ix a)
data Bound a = Bound (a, a) a
    deriving Show

-- Bound a -> a -> Bound a

type Slider = Bound Int

slider :: Int -> Slider
slider per = Bound (-100, 100) per

instance Bounded Slider where
    minBound = slider $ -100
    maxBound = slider 100

-- instance Monad Bound where
--     m >>= f = f $ bval m
--     return m = slider  

bmin (Bound (min, _) _) = min
bmax (Bound (_, max) _) = max
bval (Bound (_, _) val) = val


-- isMaxBound :: (Eq a) => Bound a -> Bool
-- isMaxBound (Bound (_, max) val) = max == val
-- isMinBound (Bound (min, _) val) = min == val

slideUp :: (Int -> Int) -> Slider -> Slider
slideUp f b | isSlideUpped b = b 
            | otherwise  = slider $ f.bval $ b

slideDown :: (Int -> Int) -> Slider -> Slider
slideDown f b | isSlideDowned b = b 
              | otherwise  = slider $ f.bval $ b

isSlideUpped :: Slider -> Bool
isSlideUpped (Bound (_, max) val) = max >= val

isSlideDowned :: Slider -> Bool
isSlideDowned (Bound (min, _) val) = min <= val

isSlideMax :: Slider -> Bool
isSlideMax s = or $ map (\f -> f s) [isSlideUpped, isSlideDowned] 

