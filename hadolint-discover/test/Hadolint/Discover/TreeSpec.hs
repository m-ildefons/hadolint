module Hadolint.Discover.TreeSpec (spec) where

import Hadolint.Discover.Tree as Tree
import Test.Hspec


spec :: Spec
spec = do
  describe "directory tree discovery" $ do
    it "check valid module names: empty name" $ do
      isValidModuleName [] `shouldBe` False

    it "check valid module names: lower case first char" $ do
      isValidModuleName "foo" `shouldBe` False

    it "check valid module names: contains illegal char" $ do
      isValidModuleName "Fo.o" `shouldBe` False

    it "check valid module names: valid name" $ do
      isValidModuleName "Foo" `shouldBe` True
