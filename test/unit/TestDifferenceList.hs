
module TestDifferenceList where

import Test.Hspec
import HaskellProgrammingFromFirstPrinciples.Chapter29Exercises as M

infixr `dlistShouldBe`
dlistShouldBe :: (Show a, Eq a) => DList a -> [a] -> Expectation
dlistShouldBe d exp = toList d `shouldBe` exp


spec :: Spec
spec = do
    describe "Prelude.head" $ do
        it "I can create an empty list" $ do
            M.empty `dlistShouldBe` ([]::[String])
        
        it "I can create a singleton" $ do
            M.singleton "hello" `dlistShouldBe` ["hello"]

        it "I can prepend an element using cons" $ do
            M.cons "hello" (M.singleton "world") `dlistShouldBe` ["hello", "world"]
                
        it "I can append an element using snoc" $ do
            M.snoc (M.singleton "hello") "world" `dlistShouldBe` ["hello", "world"]
        
        it "I can append a list to another one" $ do
            M.append (M.singleton "hello") (M.singleton "world") `dlistShouldBe` ["hello", "world"]
