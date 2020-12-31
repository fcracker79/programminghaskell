module JSONFileSearchSpec where


import Test.Hspec
import qualified BookOfMonads.Chapter11.JSONFileSearch as J
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.Maybe as M
import qualified Data.Aeson as J

spec :: Spec
spec = do
    describe "Prelude.head" $ do
        it "I must be able to fetch numbers" $ do
            let confNumber = J.JSONSearchCriteria {
                J.jsonPath = "a.b.c", 
                J.jsonOrigin = J.JSONContent "{\"a\": {\"b\": {\"c\": 12345, \"d\": \"a string\"}}}"
            }
            v <- M.runMaybeT (R.runReaderT J.searchJSONElement confNumber)
            v `shouldBe` Just (J.Number 12345)
        it "I must be able to fetch strings" $ do
            let confString = J.JSONSearchCriteria {
                J.jsonPath = "a.b.d", 
                J.jsonOrigin = J.JSONContent "{\"a\": {\"b\": {\"c\": 12345, \"d\": \"a string\"}}}"
            }
            v <- M.runMaybeT (R.runReaderT J.searchJSONElement confString)
            v `shouldBe` Just (J.String "a string")
