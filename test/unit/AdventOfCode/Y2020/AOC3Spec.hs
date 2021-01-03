module AdventOfCode.Y2020.AOC3Spec where



import Test.Hspec
import AdventOfCode.Y2020.AOC3
import Control.Monad.Trans.State.Lazy(execState)


spec :: Spec
spec = do
    describe "Prelude.head" $ do
        it "I don't find any tree" $ do
            let area = parseArea [
                        "..##.......",
                        "#...#...#..",
                        ".#....#..#.",
                        "..#.#...#.#",
                        ".#...##..#.",
                        "..#.##.....",
                        ".#.#.#....#",
                        ".#........#",
                        "#.##...#...",
                        "#...##....#",
                        ".#..#...#.#"
                    ]
            let Just justArea = area
            let state = execState (moveUntilEnd (1, 3)) (initialState justArea)
            trees state `shouldBe` 0
        it "I find the right path" $ do
            let area = parseArea [
                        "..##.......",
                        "#...#...#..",
                        ".#....#..#.",
                        "..#.#...#.#",
                        ".#...##..#.",
                        "..#.##.....",
                        ".#.#.#....#",
                        ".#........#",
                        "#.##...#...",
                        "#...##....#",
                        ".#..#...#.#"
                    ]
            let Just justArea = area
            let state = execState (moveUntilEnd (3, 1)) (initialState justArea)
            trees state `shouldBe` 7
