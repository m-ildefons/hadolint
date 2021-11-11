module Hadolint.Rule.DL3026 (rule) where

import qualified Data.Set as Set
import Hadolint.Config.Configuration (Configuration (..))
import Hadolint.Rule
import Language.Docker.Syntax

rule :: Configuration -> Rule args
rule config = customRule check (emptyState Set.empty)
  where
    code = "DL3026"
    severity = DLErrorC
    message = "Use only an allowed registry in the FROM image"

    check line st (From BaseImage {image, alias}) =
      let newState = st |> modify (Set.insert alias)
       in if doCheck (state st) image
            then newState
            else newState |> addFail CheckFailure {..}
    check _ st _ = st

    doCheck st img = Set.member (toImageAlias img) st || Set.null allowed || isAllowed img

    toImageAlias = Just . ImageAlias . imageName
    isAllowed Image {registryName = Just registry} = Set.member registry allowed
    isAllowed Image {registryName = Nothing, imageName} =
      imageName == "scratch"
        || Set.member "docker.io" allowed
        || Set.member "hub.docker.com" allowed

    allowed :: Set.Set Registry
    allowed = allowedRegistries config
{-# INLINEABLE rule #-}
