module Hadolint.Discover.RunSpec (spec) where

import Hadolint.Discover.Run as Run
import System.IO.Silently
import System.IO (stderr)
import Test.Hspec
import Test.Mockery.Directory


spec :: Spec
spec = do
  describe "generate rules" $ around_ inTempDirectory $ do
    it "generate an autorule" $ do
      touch "Rule/DL9000.hs"
      touch "Rule/DL9001.hs"
      touch "Rule/DL9002.hs"

      run [ "Rule/Auto.hs", "", "out" ]
      readFile "out" `shouldReturn`
        unlines
          [ "{-# LINE 1 \"Rule/Auto.hs\" #-}",
            "module Rule.Auto (autorule) where",
            "import Hadolint.Config.Configuration (Configuration)",
            "import Hadolint.Rule",
            "import Hadolint.Shell (ParsedShell)",
            "import qualified Hadolint.Rule.DL9000",
            "import qualified Hadolint.Rule.DL9001",
            "import qualified Hadolint.Rule.DL9002",
            "",
            "",
            "autorule :: Configuration -> Rule ParsedShell",
            "autorule conf =",
            "  Hadolint.Rule.DL9000.rule conf",
            "    <> Hadolint.Rule.DL9001.rule conf",
            "    <> Hadolint.Rule.DL9002.rule conf",
            ""
          ]

    it "ignore non DL(.*).hs sources" $ do
      touch "Rule/DL9000.hs"
      touch "Rule/DL9001.hs"
      touch "Rule/DL9002.hs"
      touch "Rule/SomethingElse.hs"

      run [ "Rule/Auto.hs", "", "out" ]
      readFile "out" `shouldReturn`
        unlines
          [ "{-# LINE 1 \"Rule/Auto.hs\" #-}",
            "module Rule.Auto (autorule) where",
            "import Hadolint.Config.Configuration (Configuration)",
            "import Hadolint.Rule",
            "import Hadolint.Shell (ParsedShell)",
            "import qualified Hadolint.Rule.DL9000",
            "import qualified Hadolint.Rule.DL9001",
            "import qualified Hadolint.Rule.DL9002",
            "",
            "",
            "autorule :: Configuration -> Rule ParsedShell",
            "autorule conf =",
            "  Hadolint.Rule.DL9000.rule conf",
            "    <> Hadolint.Rule.DL9001.rule conf",
            "    <> Hadolint.Rule.DL9002.rule conf",
            ""
          ]

  describe "edge cases" $ around_ inTempDirectory $ do
    xit "fail with error message for incorrect arguments" $ do
      touch "Rule/fobar"  -- needed to create Rule/ directory

      (err, _) <- hCapture [stderr] $ run [ ]
      err `shouldBe`
        unlines
          [ "USAGE",
            ""
          ]

    it "don't fail if no rule can be found" $ do
      touch "Rule/fobar"  -- needed to create Rule/ directory

      run [ "Rule/Auto.hs", "", "out" ]
      readFile "out" `shouldReturn`
        unlines
          [ "{-# LINE 1 \"Rule/Auto.hs\" #-}",
            "module Rule.Auto (autorule) where",
            "import Hadolint.Config.Configuration (Configuration)",
            "import Hadolint.Rule",
            "import Hadolint.Shell (ParsedShell)",
            "",
            "",
            "autorule :: Configuration -> Rule ParsedShell",
            "autorule conf =",
            "  simpleRule \"DL0000\" DLIgnoreC \"\" (\\_ -> True)",
            ""
          ]
