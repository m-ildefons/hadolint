module Hadolint.Rule.DL3038 (rule) where

import Hadolint.Config.Configuration (Configuration)
import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

rule :: Configuration -> Rule Shell.ParsedShell
rule _ = simpleRule code severity message check
  where
    code = "DL3038"
    severity = DLWarningC
    message = "Use the -y switch to avoid manual input `dnf install -y <package`"

    check (Run (RunArgs args _)) = foldArguments (Shell.noCommands forgotDnfYesOption) args
    check _ = True

    forgotDnfYesOption cmd = isDnfInstall cmd && not (hasYesOption cmd)
    isDnfInstall = Shell.cmdHasArgs "dnf" ["install", "groupinstall", "localinstall"]
    hasYesOption = Shell.hasAnyFlag ["y", "assumeyes"]
{-# INLINEABLE rule #-}
