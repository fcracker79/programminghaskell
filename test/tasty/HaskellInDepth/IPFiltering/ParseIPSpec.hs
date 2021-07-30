module HaskellInDepth.IPFiltering.ParseIPSpec where

import Test.Hspec
import HaskellInDepth.IPFiltering.IPTypes
import HaskellInDepth.IPFiltering.ParseIP
spec_buildIP :: Spec
spec_buildIP =
    describe "buildIP" $ do
        it "builds from zero" $ buildIP [0,0,0,0] `shouldBe` IP 0
        it "builds from one" $ buildIP [0,0,0,1] `shouldBe` IP 1
        it "builds from localhost" $ buildIP [127,0,0,1] `shouldBe` IP (1 + 127 * 256^3)
        it "builds from arbitrary address" $ buildIP [192,168,3,15] `shouldBe` IP (15 + 3 * 256 + 168 * 256^2 + 192 * 256^3)
