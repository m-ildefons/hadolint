module Hadolint.Formatter.SarifSpec (spec) where

import Helpers
import Data.List.NonEmpty as NonEmpty
import Hadolint
  ( CheckFailure (..),
    DLSeverity (..),
    OutputFormat (..),
    Result (..),
    getShortVersion,
  )
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?noColor = True

  describe "Formatter: Sarif" $ do
    it "print empty results" $ do
      let checkFails =
            NonEmpty.fromList
              [ Result
                  { fileName = "<string>",
                    errors = mempty,
                    checks = mempty
                  }
              ]
          expectation = "{\"runs\":[{\"defaultSourceLanguage\":\"dockerfi\
                        \le\",\"results\":[],\"tool\":{\"driver\":{\"full\
                        \Name\":\"Haskell Dockerfile Linter\",\"downloadU\
                        \ri\":\"https://github.com/hadolint/hadolint\",\"\
                        \shortDescription\":{\"text\":\"Dockerfile linter\
                        \, validate inline bash, written in Haskell\"},\"\
                        \name\":\"Hadolint\",\"version\":\""
                        ++ getShortVersion
                        ++ "\"}}}],\"version\":\"2.1.0\",\"$schema\
                           \\":\"http://json.schemastore.org/sarif-2.1.0\"}"
      assertFormatter Sarif checkFails expectation

    it "print syntax error" $ do
      let checkFails = 
            NonEmpty.fromList
              [ Result
                  { fileName = "<string>",
                    errors = mempty,
                    checks =
                      [ CheckFailure
                           { code = "DL1000",
                             severity = DLErrorC,
                             message = "",
                             line = 3
                           }
                       ] } ]
          expectation = "{\"runs\":[{\"defaultSourceLanguage\":\"dockerfi\
                        \le\",\"results\":[{\"ruleId\":\"DL1000\",\"message\":\
                        \{\"text\":\"\"},\"level\":\"error\",\"\
                        \locations\":[{\"physicalLocation\":{\"region\":{\"\
                        \sourceLanguage\":\"dockerfile\",\"startLine\":3,\"\
                        \endLine\":3,\"startColumn\":1,\"endColumn\":1},\"\
                        \artifactLocation\":{\"uri\":\"<string>\"}}}]}],\"\
                        \tool\":{\"driver\":{\"full\
                        \Name\":\"Haskell Dockerfile Linter\",\"downloadU\
                        \ri\":\"https://github.com/hadolint/hadolint\",\"\
                        \shortDescription\":{\"text\":\"Dockerfile linter\
                        \, validate inline bash, written in Haskell\"},\"\
                        \name\":\"Hadolint\",\"version\":\""
                        ++ getShortVersion
                        ++ "\"}}}],\"version\":\"2.1.0\",\"$schema\
                           \\":\"http://json.schemastore.org/sarif-2.1.0\"}"
      assertFormatter Sarif checkFails expectation

    it "print one  results" $ do
      let checkFails =
            NonEmpty.fromList
              [ Result
                  { fileName = "<string>",
                    errors = mempty,
                    checks =
                      [ CheckFailure
                           { code = "DL2001",
                             severity = DLInfoC,
                             message = "test",
                             line = 1
                           }
                      ]
                  }
              ]
          expectation = "{\"runs\":[{\"defaultSourceLanguage\":\"dockerfi\
                        \le\",\"results\":[{\"ruleId\":\"DL2001\",\"message\":\
                        \{\"text\":\"test\"},\"level\":\"note\",\"\
                        \locations\":[{\"physicalLocation\":{\"region\":{\"\
                        \sourceLanguage\":\"dockerfile\",\"startLine\":1,\"\
                        \endLine\":1,\"startColumn\":1,\"endColumn\":1},\"\
                        \artifactLocation\":{\"uri\":\"<string>\"}}}]}],\"\
                        \tool\":{\"driver\":{\"full\
                        \Name\":\"Haskell Dockerfile Linter\",\"downloadU\
                        \ri\":\"https://github.com/hadolint/hadolint\",\"\
                        \shortDescription\":{\"text\":\"Dockerfile linter\
                        \, validate inline bash, written in Haskell\"},\"\
                        \name\":\"Hadolint\",\"version\":\""
                        ++ getShortVersion
                        ++ "\"}}}],\"version\":\"2.1.0\",\"$schema\
                           \\":\"http://json.schemastore.org/sarif-2.1.0\"}"
      assertFormatter Sarif checkFails expectation

    it "print many results" $ do
      let checkFails =
              NonEmpty.fromList
              [ Result
                  { fileName = "<string>",
                    errors = mempty,
                    checks =
                      [ CheckFailure
                           { code = "DL2001",
                             severity = DLErrorC,
                             message = "test 1",
                             line = 1
                           },
                         CheckFailure
                           { code = "DL2002",
                             severity = DLInfoC,
                             message = "test 2",
                             line = 2
                           }
                       ]
                  }
              ]
          expectation = "{\"runs\":[{\"defaultSourceLanguage\":\"dockerfi\
                        \le\",\"results\":[{\"ruleId\":\"DL2001\",\"message\":\
                        \{\"text\":\"test 1\"},\"level\":\"error\",\"\
                        \locations\":[{\"physicalLocation\":{\"region\":{\"\
                        \sourceLanguage\":\"dockerfile\",\"startLine\":1,\"\
                        \endLine\":1,\"startColumn\":1,\"endColumn\":1},\"\
                        \artifactLocation\":{\"uri\":\"<string>\"}}}]},{\"\
                        \ruleId\":\"DL2002\",\"message\":{\"text\":\"test 2\"\
                        \},\"level\":\"note\",\"locations\":[{\"physicalLocati\
                        \on\":{\"region\":{\"sourceLanguage\":\"dockerfile\"\
                        \,\"startLine\":2,\"endLine\":2,\"startColumn\":1\
                        \,\"endColumn\":1},\"artifactLocation\":{\"uri\":\"\
                        \<string>\"}}}]}],\"tool\":{\"driver\":{\"full\
                        \Name\":\"Haskell Dockerfile Linter\",\"downloadU\
                        \ri\":\"https://github.com/hadolint/hadolint\",\"\
                        \shortDescription\":{\"text\":\"Dockerfile linter\
                        \, validate inline bash, written in Haskell\"},\"\
                        \name\":\"Hadolint\",\"version\":\""
                        ++ getShortVersion
                        ++ "\"}}}],\"version\":\"2.1.0\",\"$schema\
                           \\":\"http://json.schemastore.org/sarif-2.1.0\"}"
      assertFormatter Sarif checkFails expectation
