module Hadolint.Config.ConfigurationSpec where

import Data.Default
import qualified Data.Map as Map
import qualified Data.Set as Set
import Hadolint
import Test.Hspec

spec :: SpecWith ()
spec = do
  describe "Configuration" $ do
    it "default configuration" $ do
      def
        `shouldBe` Configuration
          False
          False
          False
          TTY
          mempty
          mempty
          mempty
          mempty
          mempty
          mempty
          mempty
          False
          DLInfoC

    it "override default configuration with empty config" $ do
      applyPartialConfiguration def mempty
        `shouldBe` Configuration
          False
          False
          False
          TTY
          mempty
          mempty
          mempty
          mempty
          mempty
          mempty
          mempty
          False
          DLInfoC

    it "override default with specific configuration: no-fail" $ do
      let config =
            PartialConfiguration
              (Just True)
              Nothing
              Nothing
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              Nothing
              mempty
      applyPartialConfiguration def config
        `shouldBe` Configuration
          True
          False
          False
          TTY
          mempty
          mempty
          mempty
          mempty
          mempty
          mempty
          mempty
          False
          DLInfoC

    it "override default with specific configuration: no-color" $ do
      let config =
            PartialConfiguration
              Nothing
              (Just True)
              Nothing
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              Nothing
              mempty
      applyPartialConfiguration def config
        `shouldBe` Configuration
          False
          True
          False
          TTY
          mempty
          mempty
          mempty
          mempty
          mempty
          mempty
          mempty
          False
          DLInfoC

    it "empty should not override: no-color" $ do
      let config =
            PartialConfiguration
              Nothing
              (Just True)
              Nothing
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              Nothing
              mempty
          config2 =
            PartialConfiguration
              Nothing
              Nothing
              Nothing
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              Nothing
              mempty
      applyPartialConfiguration def (config <> config2)
        `shouldBe` Configuration
          False
          True
          False
          TTY
          mempty
          mempty
          mempty
          mempty
          mempty
          mempty
          mempty
          False
          DLInfoC

    it "override default with specific configuration: verbose" $ do
      let config =
            PartialConfiguration
              Nothing
              Nothing
              (Just True)
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              Nothing
              mempty
      applyPartialConfiguration def config
        `shouldBe` Configuration
          False
          False
          True
          TTY
          mempty
          mempty
          mempty
          mempty
          mempty
          mempty
          mempty
          False
          DLInfoC

    it "override default with specific configuration: output-format json" $ do
      let config =
            PartialConfiguration
              Nothing
              Nothing
              Nothing
              (Just Json)
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              Nothing
              mempty
      applyPartialConfiguration def config
        `shouldBe` Configuration
          False
          False
          False
          Json
          mempty
          mempty
          mempty
          mempty
          mempty
          mempty
          mempty
          False
          DLInfoC

    it "override default with specific configuration: error rules" $ do
      let config = mempty { partialErrorRules = [ "DL3010", "DL3020" ] }
          expected = def { errorRules = [ "DL3010", "DL3020" ] }
      applyPartialConfiguration def config `shouldBe` expected

    it "override default with specific configuration: warning rules" $ do
      let config = mempty { partialWarningRules = [ "DL3010", "DL3020" ] }
          expected = def { warningRules = [ "DL3010", "DL3020" ] }
      applyPartialConfiguration def config `shouldBe` expected

    it "override default with specific configuration: info rules" $ do
      let config = mempty { partialInfoRules = [ "DL3010", "DL3020" ] }
          expected = def { infoRules = [ "DL3010", "DL3020" ] }
      applyPartialConfiguration def config `shouldBe` expected

    it "override default with specific configuration: style rules" $ do
      let config = mempty { partialStyleRules = [ "DL3010", "DL3020" ] }
          expected = def { styleRules = [ "DL3010", "DL3020" ] }
      applyPartialConfiguration def config `shouldBe` expected

    it "override default with specific configuration: ignore rules" $ do
      let config = mempty { partialIgnoreRules = [ "DL3010", "DL3020" ] }
          expected = def { ignoreRules = [ "DL3010", "DL3020" ] }
      applyPartialConfiguration def config `shouldBe` expected

    it "override default with specific configuration: label schema" $ do
      let config =
            mempty
              { partialLabelSchema = Map.fromList [ ("email", Email) ] }
          expected = def { labelSchema = Map.fromList [ ("email", Email) ] }
      applyPartialConfiguration def config `shouldBe` expected

    it "override default with specific configuration: allowed registries" $ do
      let config =
            mempty
              { partialAllowedRegistries = Set.fromList [ "docker.io" ] }
          expected = def { allowedRegistries = Set.fromList [ "docker.io" ] }
      applyPartialConfiguration def config `shouldBe` expected

    it "override default with specific configuration: strict labels" $ do
      let config = mempty { partialStrictLabels = Just True }
          expected = def { strictLabels = True }
      applyPartialConfiguration def config `shouldBe` expected

    it "override default with specific configuration: failure-threshold " $ do
      let config = mempty { partialFailureThreshold = Just DLErrorC }
          expected = def { failureThreshold = DLErrorC }
      applyPartialConfiguration def config `shouldBe` expected
