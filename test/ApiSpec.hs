module ApiSpec (spec) where

import           Test.Hspec
import           Test.Hspec.QuickCheck

x :: Int
x = 10

spec :: Spec
spec = do
    describe "Slack API" $ do
        it "runs tests" $ do
            x `shouldBe` 10
