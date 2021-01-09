module AdventOfCode.Y2020.AOC6Spec where



import Test.Hspec
import AdventOfCode.Y2020.AOC6
import qualified Misc.MyParser as P


spec :: Spec
spec = do
    describe "Prelude.head" $ do
        it "I correctly count tye positive answers" $ do
            let Just (allPositiveAnswers, _) = P.parse parseAllPositiveAnswers "abc\n\
                \\n\
                \a\n\
                \b\n\
                \c\n\
                \\n\
                \ab\n\
                \ac\n\
                \\n\
                \a\n\
                \a\n\
                \a\n\
                \a\n\
                \\n\
                \b"
            countAllPositiveAnswers allPositiveAnswers`shouldBe` 11
