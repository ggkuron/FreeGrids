module World.Field.EntitiesSpec (main, spec) where

import Test.Hspec
import World.Field.Field

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
   describe "" $ do
       it "" $ do
            True `shouldBe` True
            nextDirect


