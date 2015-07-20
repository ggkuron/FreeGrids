module World.Data.SliderSpec (main, spec) where

import Test.Hspec
import World.Data.Slider 

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "structure" $ do
        it "bounded structure" $ do
            let s0 = slider 0
                smax = maxBound :: Slider
                smin = minBound :: Slider
            bval (slideUp (+1) s0) `shouldBe` 1
            slideUp (+1) smax `shouldBe` smax
            slideDown (flip (-) 1) smin `shouldBe` smin
                

            

