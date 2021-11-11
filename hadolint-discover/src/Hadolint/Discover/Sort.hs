module Hadolint.Discover.Sort
  ( sortNaturallyBy,
    NaturalSortKey,
    naturalSortKey
  )
where

import Control.Arrow hiding (first)
import Data.Bifunctor (first)
import Data.Char
import Data.List


newtype NaturalSortKey = NaturalSortKey [Chunk]
  deriving (Eq, Ord)

data Chunk = Numeric Integer Int | Textual [(Char, Char)]
  deriving (Eq, Ord)


sortNaturallyBy :: (a -> (String, Int)) -> [a] -> [a]
sortNaturallyBy f = sortOn (first naturalSortKey . f)

naturalSortKey :: String -> NaturalSortKey
naturalSortKey = NaturalSortKey . chunks
  where
    chunks [] = []
    chunks s@(c:_)
      | isDigit c = Numeric (read num) (length num) : chunks afterNum
      | otherwise = Textual (map (toLower &&& id) str) : chunks afterStr
      where
        (num, afterNum) = span  isDigit s
        (str, afterStr) = break isDigit s
