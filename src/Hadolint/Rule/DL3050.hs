module Hadolint.Rule.DL3050 (rule) where

import qualified Data.Map as Map
import Hadolint.Config.Configuration (Configuration (..))
import Hadolint.Rule
import Language.Docker.Syntax


rule :: Configuration -> Rule args
rule Configuration {labelSchema, strictLabels} =
  simpleRule code severity message check
  where
    code = "DL3050"
    severity = DLInfoC
    message = "Superfluous label(s) present."
    check (Label pairs)
        | strictLabels = all ((`elem` Map.keys labelSchema) . fst) pairs
        | otherwise = True
    check _ = True
{-# INLINEABLE rule #-}
