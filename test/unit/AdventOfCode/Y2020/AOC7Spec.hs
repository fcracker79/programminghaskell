module AdventOfCode.Y2020.AOC7Spec where



import Test.Hspec
import AdventOfCode.Y2020.AOC7
import qualified Misc.MyParser as P
import Control.Monad.Trans.Reader(runReader)

spec :: Spec
spec = do
    describe "Prelude.head" $ do
        it "I correctly count tye positive answers" $ do
            let Just (bagRules, _) = P.parse parseBagRules "\
                \light red bags contain 1 bright white bag, 2 muted yellow bags.\n\
                \dark orange bags contain 3 bright white bags, 4 muted yellow bags.\n\
                \bright white bags contain 1 shiny gold bag.\n\
                \muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\n\
                \shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\n\
                \dark olive bags contain 3 faded blue bags, 4 dotted black bags.\n\
                \vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\n\
                \faded blue bags contain no other bags.\n\
                \dotted black bags contain no other bags."
            print bagRules
            runReader (howManyBagsCanContain SHINY_GOLD) bagRules `shouldBe` 4

