module Hadolint.Rule.DL3001 (rule) where

import qualified Data.Set as Set
import Hadolint.Config.Configuration (Configuration)
import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax (Instruction (..), RunArgs (..))

rule :: Configuration -> Rule Shell.ParsedShell
rule _ = simpleRule code severity message check
  where
    code = "DL3001"
    severity = DLInfoC
    message =
      "For some bash commands it makes no sense running them in a Docker container like `ssh`, \
      \`vim`, `shutdown`, `service`, `ps`, `free`, `top`, `kill`, `mount`, `ifconfig`"

    check (Run (RunArgs args _)) = foldArguments hasInvalid args
    check _ = True

    hasInvalid args = null [arg | arg <- Shell.findCommandNames args, Set.member arg invalidCmds]
    invalidCmds =
      Set.fromList
        [ "free",
          "kill",
          "mount",
          "ps",
          "service",
          "shutdown",
          "ssh",
          "top",
          "vim"
        ]
{-# INLINEABLE rule #-}
