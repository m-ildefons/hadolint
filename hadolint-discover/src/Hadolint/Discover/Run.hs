-- | A preprocessor that finds and combines rules.
--

module Hadolint.Discover.Run
  ( run

  -- exported for testing
  , Autorule (..)
  , importList
  , Tree (..)
  , Forest (..)
  , discover
  )
where

import Data.List
import Data.List.Split
import Data.Maybe
import Data.String
import System.Environment
import System.Exit
import System.IO

import Hadolint.Discover.Tree


instance IsString ShowS where
  fromString = showString

newtype Autorule = Autorule String
  deriving (Eq, Show)


run :: [String] -> IO ()
run args = do
  name <- getProgName
  case args of
    src : _ : dst : _ -> do
      rules <- findRules src
      writeFile dst (mkSpecModule src rules)
    _ -> do
      hPutStrLn stderr ("\nUsage: " ++ name ++ " SRC CUR DST\n")
      exitFailure

mkSpecModule :: FilePath -> Maybe [Autorule] -> String
mkSpecModule src nodes =
  ( "{-# LINE 1 " . shows src . " #-}\n"
  . showString ("module " ++ moduleName src ++ " (autorule) where\n")
  . showString "import Hadolint.Config.Configuration (Configuration)\n"
  . showString "import Hadolint.Rule\n"
  . showString "import Hadolint.Shell (ParsedShell)\n"
  . importList nodes
  . showString "\n\n"
  . showString "autorule :: Configuration -> Rule ParsedShell\n"
  . showString "autorule conf =\n"
  . formatRules nodes
  ) "\n"


moduleName :: FilePath -> String
moduleName src =
  ( justStripSuffix ".hs" . justStripPrefix "src." ) $
    intercalate "." $ splitOn "/" src

justStripSuffix :: Eq a => [a] -> [a] -> [a]
justStripSuffix sfx src = fromMaybe src (stripSuffix sfx src)

justStripPrefix :: Eq a => [a] -> [a] -> [a]
justStripPrefix pfx src = fromMaybe src (stripPrefix pfx src)

-- | Generate imports for a list of rules.
importList :: Maybe [Autorule] -> ShowS
importList = foldr ((.) . f) "". maybe [] moduleNames
  where
    f :: String -> ShowS
    f spec = "import qualified " . showString spec . "\n"

moduleNames :: [Autorule] -> [String]
moduleNames =
  concatMap fromTree
  where
    fromTree :: Autorule -> [String]
    fromTree (Autorule name) = [name]

sequenceS :: ShowS -> [ShowS] -> ShowS
sequenceS s = foldl (.) s . intersperse "    <> "

formatRules :: Maybe [Autorule] -> ShowS
formatRules = maybe nothingRule fromForest
  where
    nothingRule = "  simpleRule \"DL0000\" DLIgnoreC \"\" (\\_ -> True)\n"

    fromForest :: [Autorule] -> ShowS
    fromForest = sequenceS "  " . map fromTree

    fromTree :: Autorule -> ShowS
    fromTree (Autorule name) = showString name . ".rule conf\n"

findRules :: FilePath -> IO (Maybe [Autorule])
findRules = fmap (fmap toRules) . discover

toRules :: Forest -> [Autorule]
toRules = fromForest []
  where
    fromForest :: [String] -> Forest -> [Autorule]
    fromForest names (Forest xs) = concatMap (fromTree names) xs

    fromTree :: [String] -> Tree -> [Autorule]
    fromTree names spec = case spec of
      Leaf name -> [Autorule $ mkModule (name : names )]
      Node name forest -> fromForest (name : names) forest

mkModule :: [String] -> String
mkModule names = "Hadolint.Rule." ++ intercalate "." (reverse names)
