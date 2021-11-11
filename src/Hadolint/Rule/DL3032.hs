module Hadolint.Rule.DL3032 (rule) where

import Hadolint.Config.Configuration (Configuration)
import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

rule :: Configuration -> Rule Shell.ParsedShell
rule _ = simpleRule code severity message check
  where
    code = "DL3032"
    severity = DLWarningC
    message = "`yum clean all` missing after yum command."

    check (Run (RunArgs args _)) =
      foldArguments (Shell.noCommands yumInstall) args
        || ( foldArguments (Shell.anyCommands yumInstall) args
               && foldArguments (Shell.anyCommands yumClean) args
           )
    check _ = True

    yumInstall = Shell.cmdHasArgs "yum" ["install"]
    yumClean = Shell.cmdHasArgs "yum" ["clean", "all"]
{-# INLINEABLE rule #-}
