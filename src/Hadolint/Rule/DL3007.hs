module Hadolint.Rule.DL3007 (rule) where

import Hadolint.Config.Configuration (Configuration)
import Hadolint.Rule
import Language.Docker.Syntax

rule :: Configuration -> Rule args
rule _ = simpleRule code severity message check
  where
    code = "DL3007"
    severity = DLWarningC
    message =
      "Using latest is prone to errors if the image will ever update. Pin the version explicitly \
      \to a release tag"
    check (From BaseImage {tag = Just t}) = t /= "latest"
    check _ = True
{-# INLINEABLE rule #-}
