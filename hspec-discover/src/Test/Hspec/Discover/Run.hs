{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | A preprocessor that finds and combines specs.
--
-- /NOTE:/ This module is not meant for public consumption.  For user
-- documentation look at http://hspec.github.io/hspec-discover.html.
module Test.Hspec.Discover.Run (
  run

-- exported for testing
, Spec(..)
, importList
, driverWithFormatter
, moduleNameFromId
, pathToModule
, Tree(..)
, Forest(..)
, Hook(..)
, discover
) where
import           Control.Monad
import           Control.Applicative
import           Data.List
import           Data.Char
import           Data.Maybe
import           Data.String
import           System.Environment
import           System.Exit
import           System.IO
import           System.Directory (doesDirectoryExist, getDirectoryContents, doesFileExist)
import           System.FilePath hiding (combine)

import           Test.Hspec.Discover.Config
import           Test.Hspec.Discover.Sort

instance IsString ShowS where
  fromString = showString

data Spec = Spec {
  specModule :: String
} deriving (Eq, Show)

run :: [String] -> IO ()
run args_ = do
  name <- getProgName
  case args_ of
    src : _ : dst : args -> case parseConfig name args of
      Left err -> do
        hPutStrLn stderr err
        exitFailure
      Right conf -> do
        when (configNested conf)             (hPutStrLn stderr "hspec-discover: WARNING - The `--nested' option is deprecated and will be removed in a future release!")
        when (configNoMain conf)             (hPutStrLn stderr "hspec-discover: WARNING - The `--no-main' option is deprecated and will be removed in a future release!")
        when (isJust $ configFormatter conf) (hPutStrLn stderr "hspec-discover: WARNING - The `--formatter' option is deprecated and will be removed in a future release!")
        specs <- findSpecs src
        writeFile dst (mkSpecModule src conf specs)
    _ -> do
      hPutStrLn stderr (usage name)
      exitFailure

mkSpecModule :: FilePath -> Config -> Maybe [Spec] -> String
mkSpecModule src conf nodes =
  ( "{-# LINE 1 " . shows src . " #-}\n"
  . showString "{-# LANGUAGE NoImplicitPrelude #-}\n"
  . showString "{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}\n"
  . showString ("module " ++ moduleName src conf ++" where\n")
  . importList nodes
  . showString "import Test.Hspec.Discover\n"
  . maybe driver driverWithFormatter (configFormatter conf)
  . showString "spec :: Spec\n"
  . showString "spec = "
  . formatSpecs nodes
  ) "\n"
  where
    driver =
        case configNoMain conf of
          False ->
              showString "main :: IO ()\n"
            . showString "main = hspec spec\n"
          True -> ""

moduleName :: FilePath -> Config -> String
moduleName src conf = fromMaybe (if configNoMain conf then pathToModule src else "Main") (configModuleName conf)

-- | Derive module name from specified path.
pathToModule :: FilePath -> String
pathToModule f = toUpper m:ms
  where
    fileName = last $ splitDirectories f
    m:ms = takeWhile (/='.') fileName

driverWithFormatter :: String -> ShowS
driverWithFormatter f =
    showString "import qualified " . showString (moduleNameFromId f) . showString "\n"
  . showString "main :: IO ()\n"
  . showString "main = hspecWithFormatter " . showString f . showString " spec\n"

-- | Return module name of a fully qualified identifier.
moduleNameFromId :: String -> String
moduleNameFromId = reverse . dropWhile (== '.') . dropWhile (/= '.') . reverse

-- | Generate imports for a list of specs.
importList :: Maybe [Spec] -> ShowS
importList = foldr (.) "" . map f . fromMaybe []
  where
    f :: Spec -> ShowS
    f spec = "import qualified " . showString (specModule spec) . "Spec\n"

-- | Combine a list of strings with (>>).
sequenceS :: [ShowS] -> ShowS
sequenceS = foldr (.) "" . intersperse " >> "

-- | Convert a list of specs to code.
formatSpecs :: Maybe [Spec] -> ShowS
formatSpecs specs = case specs of
  Nothing -> "return ()"
  Just xs -> sequenceS (map formatSpec xs)

-- | Convert a spec to code.
formatSpec :: Spec -> ShowS
formatSpec (Spec name) = "describe " . shows name . " " . showString name . "Spec.spec"

findSpecs :: FilePath -> IO (Maybe [Spec])
findSpecs = fmap (fmap toSpecs) . discover

toSpecs :: Forest -> [Spec]
toSpecs = fromForest []
  where
    fromForest :: [String] -> Forest -> [Spec]
    fromForest names (Forest hook xs) = concatMap (fromTree names) xs

    fromTree :: [String] -> Tree -> [Spec]
    fromTree names spec = case spec of
      Leaf name -> [Spec . intercalate "." $ reverse (name : names )]
      Node name forest -> fromForest (name : names) forest

-- See `Cabal.Distribution.ModuleName` (http://git.io/bj34)
isValidModuleName :: String -> Bool
isValidModuleName [] = False
isValidModuleName (c:cs) = isUpper c && all isValidModuleChar cs

isValidModuleChar :: Char -> Bool
isValidModuleChar c = isAlphaNum c || c == '_' || c == '\''

data Tree = Leaf String | Node String Forest
  deriving (Eq, Show)

data Forest = Forest Hook [Tree]
  deriving (Eq, Show)

data Hook = WithHook | WithoutHook
  deriving (Eq, Show)

discover :: FilePath -> IO (Maybe Forest)
discover src = (>>= filterSrc) <$> specForest dir
  where
    filterSrc :: Forest -> Maybe Forest
    filterSrc (Forest hook xs) = ensureForest hook $ maybe id (filter . (/=)) (toSpec file) xs

    (dir, file) = splitFileName src

specForest :: FilePath -> IO (Maybe Forest)
specForest dir = do
  files <- filterModules <$> listDirectory dir
  hook <- mkHook dir files
  ensureForest hook . catMaybes <$> mapM toSpecTree files
  where
    filterModules :: [FilePath] -> [FilePath]
    filterModules = sortNaturallyBy takeBaseName . filter (isValidModuleName . takeBaseName)

    toSpecTree :: FilePath -> IO (Maybe Tree)
    toSpecTree name = do
      isDirectory <- doesDirectoryExist (dir </> name)
      if isDirectory then do
        xs <- specForest (dir </> name)
        return $ Node name <$> xs
      else do
        isFile <- doesFileExist (dir </> name)
        return $ guard isFile >> toSpec name

mkHook :: FilePath -> [FilePath] -> IO Hook
mkHook dir files
  | "SpecHook.hs" `elem` files = do
    isFile <- doesFileExist (dir </> "SpecHook.hs")
    return $ if isFile then WithHook else WithoutHook
  | otherwise = return WithoutHook

toSpec :: FilePath -> Maybe Tree
toSpec file = Leaf <$> (stripSuffix "Spec.hs" file <|> stripSuffix "Spec.lhs" file)
  where
    stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
    stripSuffix suffix str = reverse <$> stripPrefix (reverse suffix) (reverse str)

ensureForest :: Hook -> [Tree] -> Maybe Forest
ensureForest hook = fmap (Forest hook) . ensureNotNull

ensureNotNull :: [a] -> Maybe [a]
ensureNotNull xs = guard (not . null $ xs) >> Just xs

listDirectory :: FilePath -> IO [FilePath]
listDirectory path = filter f <$> getDirectoryContents path
  where f filename = filename /= "." && filename /= ".."
