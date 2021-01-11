module AdventOfCode.Y2020.AOC8Spec where



import Test.Hspec
import AdventOfCode.Y2020.AOC8
import qualified Misc.MyParser as P
import Control.Monad.Trans.Reader(runReader)

spec :: Spec
spec = do
    describe "Prelude.head" $ do
        it "I correctly count the accumulator value" $ do
            let Just (instructions, _) = P.parse parseInstructions "\
                \nop +0\n\
                \acc +1\n\
                \jmp +4\n\
                \acc +3\n\
                \jmp -3\n\
                \acc -99\n\
                \acc +1\n\
                \jmp -4\n\
                \acc +6\n"
            accumulatorValue instructions `shouldBe` 5
