module World.DataSpec (main, spec) where

import Test.Hspec
import World.Data

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "cellEdge" $ do
        it "row is a first field" $ do
            celledge UpperLeft (Cell (5:!:5)) `shouldBe` (Cell(5:!:5))
            celledge LowerLeft (Cell (5:!:5)) `shouldBe` (Cell(6:!:5))
            celledge UpperRight (Cell (5:!:5)) `shouldBe` (Cell(5:!:6))
            celledge LowerRight (Cell (5:!:5)) `shouldBe` (Cell(6:!:6))
    describe "cellMoves" $ do
        it "turn to origin" $ do
            let cm c = cellMoves allDirection [] c `shouldBe` c
            cm (Cell(5:!:5))
    describe "slider" $ do
        it "" $ do
            slideRCoord center RIGHT `shouldNotBe` center


