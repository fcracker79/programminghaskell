module NewChristmasTreeSpec where


import Test.Hspec
import BookOfMonads.Chapter11.ChristmasTree (newChristmasTree)

spec :: Spec
spec = do
    describe "Prelude.head" $ do
        it "I have the right 5 height Christmas tree" $ do
            
            newChristmasTree 5 `shouldBe` Just "   *\n\
                                               \  ***\n\
                                               \ *****\n\
                                               \*******\n\
                                               \   |"
        it "The 0 height tree returns an empty string" $ do
            newChristmasTree 0 `shouldBe` Just ""

        it "The 1 height tree returns a trunk" $ do
            newChristmasTree 1 `shouldBe` Just "|"
        
        it "The 2 height tree returns a 1-star tree" $ do
            newChristmasTree 2 `shouldBe` Just "*\n|"
        
        it "Negative height tree does not exist" $ do
            mconcat <$> (mapM (\x -> newChristmasTree x `shouldBe` Nothing) [-1000..(-1)])
