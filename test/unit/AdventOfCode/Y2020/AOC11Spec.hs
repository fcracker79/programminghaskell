module AdventOfCode.Y2020.AOC11Spec where



import Test.Hspec
import AdventOfCode.Y2020.AOC11

spec :: Spec
spec = do
    describe "Prelude.head" $ do
        it "I correctly count the number of states" $ do
            turnsCount (createSeats 10 
                "L.LL.LL.LL\
                \LLLLLLL.LL\
                \L.L.L..L..\
                \LLLL.LL.LL\
                \L.LL.LL.LL\
                \L.LLLLL.LL\
                \..L.L.....\
                \LLLLLLLLLL\
                \L.LLLLLL.L\
                \L.LLLLL.LL") `shouldBe` 37
