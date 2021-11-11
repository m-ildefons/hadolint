module Hadolint.Rule.DL3040 (rule) where

import Hadolint.Config.Configuration (Configuration)
import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

rule :: Configuration -> Rule Shell.ParsedShell
rule _ = simpleRule code severity message check
  where
    code = "DL3040"
    severity = DLWarningC
    message = "`dnf clean all` missing after dnf command."

    check (Run (RunArgs args _)) =
      foldArguments (Shell.noCommands dnfInstall) args
        || ( foldArguments (Shell.anyCommands dnfInstall) args
               && foldArguments (Shell.anyCommands dnfClean) args
           )
    check _ = True

    dnfInstall = Shell.cmdHasArgs "dnf" ["install"]
    dnfClean = Shell.cmdHasArgs "dnf" ["clean", "all"]
{-# INLINEABLE rule #-}
