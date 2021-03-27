
module TestQueue where

import Test.Hspec
import HaskellProgrammingFromFirstPrinciples.Chapter29Exercises as M


spec :: Spec
spec = do
    describe "Prelude.head" $ do
        it "I can append" $ do
            let q = (M.push "3" . M.push "2" . M.push "1") (M.Queue [] [])
            let Just (a1, q1) = M.pop q
            a1 `shouldBe` "1"
            let Just (a2, q2) = M.pop q1
            a2 `shouldBe` "2"
            let Just (a3, q3) = M.pop q2
            a3 `shouldBe` "3"
            M.pop q3 `shouldBe` Nothing
            