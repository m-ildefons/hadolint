module Hadolint.Meta
  ( getVersion,
    getShortVersion,
  )
where

import GitHash

gitInfo :: String
gitInfo = version <> dirty
  where
    gi = $$tGitInfoCwd
    version = giTag gi
    dirty
      | giDirty gi = "-dirty"
      | otherwise = ""

getVersion :: String
getVersion = "Haskell Dockerfile Linter " <> gitInfo

getShortVersion :: String
getShortVersion = gitInfo
