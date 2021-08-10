module Hadolint.Rule.DL3061 (rule) where

import Hadolint.Rule
import Language.Docker.Syntax
import qualified Hadolint.Shell as Shell
import qualified Data.Text as Text

rule :: Rule Shell.ParsedShell
rule = simpleRule code severity message check
  where
    code = "DL3061"
    severity = DLWarningC
    message = "Inline comment. This may disable following lines in the shell."
    check (Run (RunArgs args _)) = foldArguments hasComment args
    check _ = True
{-# INLINEABLE rule #-}

hasComment :: Shell.ParsedShell -> Bool
hasComment args =
  not ("#" `Text.isInfixOf` Shell.original args)
  || ("\n" `Text.isInfixOf` Shell.original args)
