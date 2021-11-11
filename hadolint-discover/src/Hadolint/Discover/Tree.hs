module Hadolint.Discover.Tree where

import Control.Monad
import Data.List
import Data.Char
import Data.Maybe
import System.Directory
  ( doesDirectoryExist, getDirectoryContents, doesFileExist )
import System.FilePath hiding (combine)

import Hadolint.Discover.Sort


data Tree = Leaf String | Node String Forest
  deriving (Eq, Show)

newtype Forest = Forest [Tree]
  deriving (Eq, Show)


-- See `Cabal.Distribution.ModuleName` (http://git.io/bj34)
isValidModuleName :: String -> Bool
isValidModuleName [] = False
isValidModuleName (c:cs) = isUpper c && all isValidModuleChar cs

isValidModuleChar :: Char -> Bool
isValidModuleChar c = isAlphaNum c || c == '_' || c == '\''

sortKey :: Tree -> (String, Int)
sortKey tree = case tree of
  Leaf name -> (name, 0)
  Node name _ -> (name, 1)

discover :: FilePath -> IO (Maybe Forest)
discover src = (>>= filterSrc) <$> specForest dir
  where
    filterSrc :: Forest -> Maybe Forest
    filterSrc (Forest xs) = ensureForest $
      maybe id (filter . (/=)) (toRule file) xs

    (dir, file) = splitFileName src

specForest :: FilePath -> IO (Maybe Forest)
specForest dir = do
  files <- listDirectory dir
  ensureForest . sortNaturallyBy sortKey . catMaybes <$> mapM toRuleTree files
  where
    toRuleTree :: FilePath -> IO (Maybe Tree)
    toRuleTree name
      | isValidModuleName name = do
          doesDirectoryExist (dir </> name) `fallback` Nothing $ do
            xs <- specForest (dir </> name)
            return $ Node name <$> xs
      | otherwise = do
          doesFileExist (dir </> name) `fallback` Nothing $ do
            return $ toRule name

fallback :: IO Bool -> a -> IO a -> IO a
fallback p def action = do
  bool <- p
  if bool then action else return def

toRule :: FilePath -> Maybe Tree
toRule file = Leaf <$> (rule >>= ensure isValidModuleName)
  where
    rule :: Maybe String
    rule =
      if "DL" `isPrefixOf` file
        then stripSuffix ".hs" file
        else Nothing

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suffix str = reverse <$> stripPrefix (reverse suffix) (reverse str)

ensure :: (a -> Bool) -> a -> Maybe a
ensure p a = guard (p a) >> Just a

ensureForest :: [Tree] -> Maybe Forest
ensureForest = fmap Forest . ensure (not . null)

listDirectory :: FilePath -> IO [FilePath]
listDirectory path = filter f <$> getDirectoryContents path
  where f filename = filename /= "." && filename /= ".."
