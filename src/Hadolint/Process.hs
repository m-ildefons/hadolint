module Hadolint.Process (run) where

import Hadolint.Config.Configuration (Configuration (..))
import Hadolint.Rule (CheckFailure (..), Failures, Rule, RuleCode)
import Language.Docker.Syntax
import qualified Control.Foldl as Foldl
import qualified Data.IntMap.Strict as SMap
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Hadolint.Rule.Auto
import qualified Hadolint.Pragma
import qualified Hadolint.Rule.Shellcheck
import qualified Hadolint.Shell as Shell


data AnalisisResult = AnalisisResult
  { -- | The set of ignored rules per line
    ignored :: SMap.IntMap (Set.Set RuleCode),
    -- | A set of failures collected for reach rule
    failed :: Failures
  }

run :: Configuration -> [InstructionPos Text.Text] -> Failures
run config dockerfile = Seq.filter shouldKeep failed
  where
    AnalisisResult {..} = Foldl.fold (analyze config) dockerfile

    shouldKeep CheckFailure {line, code} =
      Just True /= do
        ignoreList <- SMap.lookup line ignored
        return $ code `Set.member` ignoreList

analyze ::
  Configuration ->
  Foldl.Fold (InstructionPos Text.Text) AnalisisResult
analyze config =
  AnalisisResult
    <$> Hadolint.Pragma.ignored
    <*> Foldl.premap parseShell (failures config <> onBuildFailures config)

parseShell :: InstructionPos Text.Text -> InstructionPos Shell.ParsedShell
parseShell = fmap Shell.parseShell

onBuildFailures :: Configuration -> Rule Shell.ParsedShell
onBuildFailures config =
  Foldl.prefilter
    isOnBuild
    (Foldl.premap unwrapOnbuild (failures config))
  where
    isOnBuild InstructionPos {instruction = OnBuild {}} = True
    isOnBuild _ = False

    unwrapOnbuild inst@InstructionPos {instruction = OnBuild i} = inst {instruction = i}
    unwrapOnbuild inst = inst

failures :: Configuration -> Rule Shell.ParsedShell
failures config =
  Hadolint.Rule.Auto.autorule config
    <> Hadolint.Rule.Shellcheck.rule config
