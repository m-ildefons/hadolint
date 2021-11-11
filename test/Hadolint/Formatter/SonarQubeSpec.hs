module Hadolint.Formatter.SonarQubeSpec (spec) where

import Data.List.NonEmpty as NonEmpty
import Data.Sequence as Seq
import Data.Text (Text)
import Helpers
import Hadolint
  ( CheckFailure (..),
    DLSeverity (..),
    OutputFormat (..),
    Result (..)
  )
import Language.Docker.Parser (DockerfileError (..))
import Test.Hspec
import Text.Megaparsec.Error
import Text.Megaparsec


spec :: SpecWith ()
spec = do
  let ?noColor = True

  describe "Formatter: SonarQube" $ do
    it "print empty results" $ do
      let checkFails =
            NonEmpty.fromList
              [ Result
                  { fileName = "<string>",
                    errors = mempty,
                    checks = mempty
                  }
              ]
          expectation = "{\"issues\":[]}"
      assertFormatter SonarQube checkFails expectation

    it "print parse error" $ do
      let checkFails =
            NonEmpty.fromList
              [ Result
                  { fileName = "<string>",
                    errors =
                      Seq.fromList
                        [ ParseErrorBundle
                            { bundleErrors =
                                TrivialError
                                  0
                                  (Just (Tokens ('F' :| "")))
                                  ( Seq.fromList
                                    [ Tokens ('#' :| ""),
                                      Label ('A' :| "DD"),
                                      Label ('A' :| "RG"),
                                      Label ('C' :| "MD"),
                                      Label ('C' :| "OPY"),
                                      Label ('E' :| "NTRYPOINT"),
                                      Label ('E' :| "NV"),
                                      Label ('E' :| "XPOSE"),
                                      Label ('F' :| "ROM"),
                                      Label ('H' :| "EALTHCHECK"),
                                      Label ('L' :| "ABEL"),
                                      Label ('M' :| "AINTAINER"),
                                      Label ('O' :| "NBUILD"),
                                      Label ('R' :| "UN"),
                                      Label ('S' :| "HELL"),
                                      Label ('S' :| "TOPSIGNAL"),
                                      Label ('U' :| "SER"),
                                      Label ('V' :| "OLUME"),
                                      Label ('W' :| "ORKDIR"),
                                      Label ('a' :| " pragma"),
                                      Label ('w' :| "hitespaces"),
                                      EndOfInput
                                    ]
                                  ) :| [],
                            bundlePosState =
                              PosState
                                { pstateInput = "FOM foo",
                                  pstateOffset = 0,
                                  pstateSourcePos =
                                    SourcePos
                                      { sourceName = "<string>",
                                        sourceLine = Pos 1,
                                        sourceColumn = Pos 1
                                      },
                            pstateTabWidth = Pos 8,
                            pstateLinePrefix = ""}}],
                    checks = mempty
                  }
              ]
          expectation = "{\"issues\":[{\"ruleId\":\"DL1000\",\"engineId\":\"Hadolint\",\"severity\":\"BLOCKER\",\"type\":\"BUG\",\"primaryLocation\":{}}]}"
      assertFormatter SonarQube checkFails expectation
