module UtilsSpec where

import Test.QuickCheck
import Test.Hspec
import Utils

spec :: Spec
spec = do
  describe "mapFirst" $ do
    it "maps first" $ do
      mapFirst (+1) (1, "abc") `shouldBe` (2, "abc")