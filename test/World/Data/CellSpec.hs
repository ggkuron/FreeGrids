module World.Data.CellSpec (main, spec) where

import Test.Hspec
import World.Data.Cell

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "field access" $ do
        it "row is a first field" $ do
            let c = Cell (5 :!: 10)
            cellRow c `shouldBe` 5
            cellCol c `shouldBe` 10
    describe "intance of Num" $ do
        it "(1,2) + (3,5) = (4,7)" $ do
            (Cell (1:!:2)) + (Cell (3:!:5)) `shouldBe` (Cell(4:!:7))
        it "(1,2) - (3,5) = (-2,-3)" $ do
            (Cell (1:!:2)) - (Cell (3:!:5)) `shouldBe` (Cell((-2):!:(-3)))

