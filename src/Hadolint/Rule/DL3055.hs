module Hadolint.Rule.DL3055 (rule) where

import qualified Data.Map as Map
import qualified Data.Text as Text
import Hadolint.Config.Configuration (Configuration (..))
import Hadolint.Rule
import Language.Docker.Syntax


rule :: Configuration -> Rule args
rule Configuration {labelSchema} = mconcat $
  fmap labelIsNotGitHashRule (Map.keys (Map.filter (== GitHash) labelSchema))
{-# INLINEABLE rule #-}

labelIsNotGitHashRule :: LabelName -> Rule args
labelIsNotGitHashRule label = simpleRule code severity message check
  where
    code = "DL3055"
    severity = DLWarningC
    message = "Label `" <> label <> "` is not a valid git hash."
    check (Label ls) = null $ getBadHashLabels label ls
    check _ = True

getBadHashLabels :: LabelName -> Pairs -> Pairs
getBadHashLabels lbl prs = [(l, v) | (l, v) <- prs, l == lbl, isBadHash v]

isBadHash :: Text.Text -> Bool
isBadHash h = Text.any (`notElem` validHash) h
            || (Text.length h /= 40 && Text.length h /= 7)

validHash :: String
validHash = ['0'..'9'] ++ ['a'..'f']
