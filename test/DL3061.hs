module DL3061 (tests) where

import qualified Data.Text as Text
import Helpers
import Test.Hspec

tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL3061 - Inlline comments in RUN may disable following lines" $ do
    it "catches inlines comment" $
      let file = Text.unlines
            [ "RUN echo foo \\",
              " && echo bar # comment \\",
              " && echo broken"
            ]
       in do
          ruleCatches "DL3061" file
          onBuildRuleCatches "DL3061" file
    it "does not catch when using heredocs" $
      let file = Text.unlines
            [ "RUN <<EOF",
              "echo foo",
              "echo bar  # commend",
              "echo not broken"
            ]
       in do
          ruleCatchesNot "DL3061" file
          onBuildRuleCatchesNot "DL3061" file
