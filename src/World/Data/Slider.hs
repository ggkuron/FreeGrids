{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module World.Data.Slider 
( module World.Data.Slider
, module Control.Lens
) where

import qualified Data.Range.Range as R
import Control.Lens 

type Range = R.Range

spanRange = R.SpanRange

class Ranged a b where
    range :: a -> Range b -- (type a) holding Range 
    rangedValue :: a -> b -- retrieve ranged value (type b) from (type a)


data RangedValue a = RangedValue 
                   { rangeSize :: Range a
                   , _rangeInsideValue :: a 
                   }

makeLenses ''RangedValue

instance Ranged (RangedValue a) a where
    range :: RangedValue a -> Range a
    range = rangeSize
    rangedValue :: RangedValue a -> a
    rangedValue = _rangeInsideValue


ranged :: Ord a => Range a -> a -> RangedValue a 
ranged r v | R.inRange r v = RangedValue {rangeSize = r, _rangeInsideValue = v} 

data Slider = Slider 
            { inner_range :: Range Int
            , sliderSize :: Int
            , _percent :: RangedValue Int
            }

makeLenses ''Slider

slider :: Int -> Int -> Slider
slider max per = Slider 
               { inner_range = spanRange (-max) (max)
               , sliderSize = max
               , _percent = ranged (spanRange (-100) 100) per
               }

slideUp :: Int -> Slider -> Slider
slideUp i sl  | per >= 100 = sl&percent.rangeInsideValue .~ 100
              | otherwise  = sl&percent.rangeInsideValue +~ i
           where per = sl^.percent^.rangeInsideValue
slideDown :: Int -> Slider -> Slider
slideDown i sl | per <= -100 = sl&percent.rangeInsideValue .~ -100
               | otherwise  = sl&percent.rangeInsideValue -~ i
           where per = sl^.percent^.rangeInsideValue

isSlideUpped :: Slider -> Bool
isSlideUpped s = s^.percent^.rangeInsideValue >= 100
isSlideDowned :: Slider -> Bool
isSlideDowned s = s^.percent^.rangeInsideValue <= -100

isSlideMax :: Slider -> Bool
isSlideMax s = val <= -100 || val >= 100
    where val = s^.percent^.rangeInsideValue

