module AdventOfCode.Y2020.AOC12Spec where



import Test.Hspec
import AdventOfCode.Y2020.AOC12
import qualified Misc.MyParser as P

spec :: Spec
spec = do
    describe "Prelude.head" $ do
        it "I correctly count manhattan distance" $ do
            let Just (actions, _) = P.parse 
                                parseActions 
                                "F10\n\
                                \N3\n\
                                \F7\n\
                                \R90\n\
                                \F11"

            manhattanDistance East actions (0,0,0,0) `shouldBe` 25
