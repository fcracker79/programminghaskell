module AdventOfCode.Y2020.AOC10Spec where



import Test.Hspec
import AdventOfCode.Y2020.AOC10

spec :: Spec
spec = do
    describe "Prelude.head" $ do
        it "I correctly the silly value" $ do
            joltDifferences [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4] `shouldBe` 35
            joltDifferences [
                    28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 
                    49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 
                    7, 9, 4, 2, 34, 10, 3
                ] `shouldBe` 220