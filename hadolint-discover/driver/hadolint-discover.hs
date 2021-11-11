module Main (main) where

import           System.Environment

import           Hadolint.Discover.Run (run)

main :: IO ()
main = getArgs >>= run
