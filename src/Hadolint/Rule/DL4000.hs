module Hadolint.Rule.DL4000 (rule) where

import Hadolint.Config.Configuration (Configuration)
import Hadolint.Rule
import Language.Docker.Syntax (Instruction (..))

rule :: Configuration -> Rule args
rule _ = simpleRule code severity message check
  where
    code = "DL4000"
    severity = DLErrorC
    message = "MAINTAINER is deprecated"
    check (Maintainer _) = False
    check _ = True
{-# INLINEABLE rule #-}
