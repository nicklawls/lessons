import Test.Hspec
import System.Environment
import Lib

main :: IO ()
main = hspec $ do
    describe "Lib.getKey" $ do
        it "Should return your test key" $ do
            key <- getKey Test
            key `shouldBe` "sk_test_SyCKdzgIfWKJDB32EcGb4N0u"
        it "Should return your live key" $ do
            key <- getKey Live
            key2 <- getEnv "STRIPE_LIVE_SECRET_KEY"
            key `shouldBe` key2
