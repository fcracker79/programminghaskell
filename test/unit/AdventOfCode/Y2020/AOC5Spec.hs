module AdventOfCode.Y2020.AOC5Spec where



import Test.Hspec
import AdventOfCode.Y2020.AOC5


spec :: Spec
spec = do
    describe "Prelude.head" $ do
        it "I correctly find seat" $ do
            findBoardingPlace "FBFBBFFRLR" `shouldBe` Just Seat { row = 44, column = 5, seatId = 357 }
            findBoardingPlace "BFFFBBFRRR" `shouldBe` Just Seat { row = 70, column = 7, seatId = 567 }
            findBoardingPlace "FFFBBBFRRR" `shouldBe` Just Seat { row = 14, column = 7, seatId = 119 }
            findBoardingPlace "BBFFBBFRLL" `shouldBe` Just Seat { row = 102, column = 4, seatId = 820 }
