module AdventOfCode.Y2020.AOC2Spec where


import Test.Hspec
import AdventOfCode.Y2020.AOC2

spec :: Spec
spec = do
    describe "Prelude.head" $ do
        it "I find the right valid passwords" $ do
            countValidPasswords ["1-3 a: abcde"] `shouldBe` 1
            countValidPasswords ["1-3 b: cdefg"] `shouldBe` 0
            countValidPasswords ["2-9 c: ccccccccc"] `shouldBe` 1
            countValidPasswords ["5: 1234"] `shouldBe` 0
            countValidPasswords ["6: 123456"] `shouldBe` 1
            countValidPasswords [
                    "1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc",
                    "5: 1234", "6: 123456"
                ] `shouldBe` 3
