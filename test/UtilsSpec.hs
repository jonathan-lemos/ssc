module UtilsSpec where

import Test.QuickCheck
import Test.Hspec
import Utils

spec :: Spec
spec = do
  describe "mapFirst" $ do
    it "maps first" $ do
      mapFirst (+1) (1, "abc") `shouldBe` (2, "abc")

  describe "trimEnd" $ do
    it "trims single from end" $ do
      trimEnd (== 'c') "abc" `shouldBe` "ab"
    it "trims many from end" $ do
      trimEnd (== 'c') "abcccc" `shouldBe` "ab"
    it "trims nothing from end" $ do
      trimEnd (== 'c') "abcd" `shouldBe` "abcd"
    it "trims nothing" $ do
      trimEnd (== 'c') "" `shouldBe` ""
    it "doesnt trim middle" $ do
      trimEnd (== 'c') "abcdc" `shouldBe` "abcd"

  describe "trimStart" $ do
    it "trims single from beginning" $ do
      trimStart (== 'a') "abc" `shouldBe` "bc"
    it "trims many from beginning" $ do
      trimStart (== 'a') "aaaabc" `shouldBe` "bc"
    it "trims nothing from start" $ do
      trimStart (== 'a') "bcd" `shouldBe` "bcd"
    it "trims nothing" $ do
      trimStart (== 'a') "" `shouldBe` ""
    it "doesnt trim middle" $ do
      trimStart (== 'a') "baad" `shouldBe` "baad"

  describe "trim" $ do
    it "trims from both sides" $ do
      trim (== 'a') "aabcaa" `shouldBe` "bc"
    it "trims nothing" $ do
      trim (== 'a') "" `shouldBe` ""
    it "doesnt trim middle" $ do
      trim (== 'a') "bac" `shouldBe` "bac"
