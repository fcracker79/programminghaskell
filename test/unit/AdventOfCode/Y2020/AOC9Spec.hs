module AdventOfCode.Y2020.AOC9Spec where



import Test.Hspec
import AdventOfCode.Y2020.AOC9

spec :: Spec
spec = do
    describe "Prelude.head" $ do
        it "I correctly find the first invalid value" $ do
            findFirstInvalidElement 5 [
                    35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 
                    182, 127, 219, 299, 277, 309, 576
                ] 
                `shouldBe` Just 127
