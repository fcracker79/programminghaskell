module AdventOfCode.Y2020.AOC1Spec where


import Test.Hspec
import AdventOfCode.Y2020.AOC1

spec :: Spec
spec = do
    describe "Prelude.head" $ do
        it "I find two numbers that sum up to 2020. Return their product " $ do
            findSumUp2020 [1721, 979, 366, 299, 675, 1456] `shouldBe` Just 514579
