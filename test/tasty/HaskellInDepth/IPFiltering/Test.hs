import Test.Tasty ( defaultMain, testGroup )
import Test.Tasty.Hspec ( testSpecs )
import HaskellInDepth.IPFiltering.ParseIPSpec ( spec_buildIP )
main :: IO ()
main = do
    specs <- concat <$> mapM testSpecs [ spec_buildIP ]
    defaultMain (testGroup "All Tests" [testGroup "Specs" specs])