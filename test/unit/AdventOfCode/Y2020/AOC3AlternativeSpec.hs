module AdventOfCode.Y2020.AOC3AlternativeSpec where


import Test.Hspec
import AdventOfCode.Y2020.AOC3Alternative
import Control.Monad.Trans.State.Lazy(evalState)
import Control.Monad.Trans.Maybe(runMaybeT)

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
            evalState (runMaybeT moveUntilEnd) (initialState (1, 3) justArea) `shouldBe` Just 0
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
            evalState (runMaybeT moveUntilEnd) (initialState (3, 1) justArea) `shouldBe` Just 7
