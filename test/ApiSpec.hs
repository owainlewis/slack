module ApiSpec
  ( spec
  ) where

import Network.Slack.Api
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "Slack API" $ do
    it "should return information about slack endpoints" $ do
      (info "groups.open") `shouldBe` "Opens a private group"
  describe "Request URL generation" $ do
    it "should build a full request URL" $ do
      (makeRequest "foo.bar") `shouldBe` "https://slack.com/api/foo.bar"
