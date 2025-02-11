{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Process (readProcess)
import Control.Monad (filterM)
import Data.Char (isUpper, isAlpha)
import Data.Maybe (mapMaybe)
import Control.Exception (try, SomeException)

-- Function to extract module names from import statements
extractModuleNames :: T.Text -> [T.Text]
extractModuleNames = mapMaybe extractModuleName . T.lines
  where
    extractModuleName line
      | "import" `T.isPrefixOf` line =
          let wordsAfterImport = drop 1 (T.words line)
              capitalizedWords = takeWhile (\w -> not (T.null w) && isUpper (T.head w) && T.all (\c -> isAlpha c || c == '.') w) wordsAfterImport
          in if not (null capitalizedWords)
             then Just $ T.intercalate "." capitalizedWords
             else Nothing
      | otherwise = Nothing

-- List of module prefixes and their corresponding package names
--prefixMappings :: [(T.Text, T.Text)]
--prefixMappings =
--    [ ("Data.Csv.", "cassava")
--    -- Add more prefix mappings as needed
--    ]





-- Function to map module names to package names
moduleToPackage :: T.Text -> T.Text
moduleToPackage modName = case modName of
--    case lookupPrefix modName prefixMappings of
--        Just pkg -> pkg
--        Nothing -> case modName of
--processModule :: Maybe T.Text -> T.text
--processModule Nothing = "unknown"
--processModule (Just modName) = case modName of  
            -- Standard library modules
            "Prelude" -> "base"
            "Control.Applicative" -> "base"
            "Control.Monad" -> "base"
            "Control.Monad.IO.Class" -> "base"
            "Control.Exception" -> "base"
            "Data.Bool" -> "base"
            "Data.Char" -> "base"
            "Data.List" -> "base"
            "Data.Maybe" -> "base"
            "Data.Either" -> "base"
            "Data.Tuple" -> "base"
            "Data.Int" -> "base"
            "Data.Word" -> "base"
            "Data.Function" -> "base"
            "Data.Functor" -> "base"
            "Data.Traversable" -> "base"
            "Data.Foldable" -> "base"
            "System.IO" -> "base"
            "System.Environment" -> "base"
            "System.Directory" -> "base"
            "System.FilePath" -> "base"
            "System.Process" -> "base"
            "Numeric" -> "base"
            "Text.Printf" -> "base"
            "Text.Read" -> "base"
            "Text.Show" -> "base"
            "GHC.Base" -> "base"
            "GHC.Enum" -> "base"
            "GHC.Float" -> "base"
            "GHC.Num" -> "base"
            "GHC.Real" -> "base"
             

            -- Add other standard modules as needed


            --Text Processing
            "Data.Text" -> "text"
            "Data.Text.Lazy" -> "text"
            "Control.Monad" -> "mtl"
            "Data.ByteString" -> "bytestring"
            "Data.ByteString.Lazy" -> "bytestring"
            --Data Structures and Handlers
            "Data.Map" -> "containers"
            "Data.Set" -> "containers"
            "Data.Sequence" -> "containers"
            "Data.Vector" -> "vector"
            "Data.Aeson" -> "aeson"
            "Data.Yaml" -> "yaml"
            "Data.Conduit" -> "conduit"
            "Data.Attoparsec" -> "attoparsec"
            "Data.Scientific" -> "scientific"
            "Data.Time" -> "time"
            "Data.UUID" -> "uuid"
            "Data.CaseInsensitive" -> "case-insensitive"
            "Data.HashMap.Strict" -> "unordered-containers"
            "Data.HashSet" -> "unordered-containers"
            --Parsing
            "Text.Parsec" -> "parsec"
            "Text.Megaparsec" -> "megaparsec"
            "Text.Printf" -> "base"
            "Text.Regex" -> "regex-compat"
            "Text.Regex.PCRE" -> "regex-pcre"

            --Concurrency
            "Control.Lens" -> "lens"
            "Control.Concurrent.STM" -> "stm"
            "Control.Concurent.Async" -> "async"
            --File System
            "System.FilePath" -> "filepath"
            "System.Directory" -> "directory"
            "System.Random" -> "random"
            --Networking
            "Network.HTTP" -> "HTTP"
            "Network.HTTP.Simple" -> "HTTP"
            "Network.HTTP.Client" -> "http-client"
            "Network.HTTP.Conduit" -> "http-conduit"
            "Network.Wreq" -> "wreq"
            "Network" -> "network"
            --Databases
            "Database.PostgreSQL.Simple" -> "postgresql-simple"
            "Database.SQLite.Simple" -> "sqlite-simple"
            "Database.Persist" -> "persistent"
            --Testing
            "Test.HUnit" -> "HUnit"
            "Test.QuickCheck" -> "QuickCheck"
            "Test.Tasty" -> "tasty"
            "Test.Tasty.HUnit" -> "tasty-hunit"
            "Test.Tasty.QuickCheck" -> "tasty-quickcheck"
            --CLI
            -- Command-Line Options
            "Options.Applicative" -> "optparse-applicative"
            -- Logging
            "System.Log.Logger" -> "hslogger"
            -- XML Processing
            "Text.XML" -> "xml"
            "Text.XML.HXT.Core" -> "hxt"
            -- Web Frameworks
            "Yesod" -> "yesod"
            "Snap" -> "snap"    
            --HPC/GPU

            --Default case
            _ -> modName
--             _ -> "unknown"

-- Helper function to find the package name based on module prefix
--lookupPrefix :: T.Text -> [(T.Text, T.Text)] -> Maybe T.Text
--lookupPrefix modName = fmap snd . find (\(prefix, _) -> prefix `T.isPrefixOf` modName)



-- Function to search for a package in NixOS packages
searchNixPackage :: T.Text -> IO Bool
searchNixPackage pkg = do
    let searchTerm = "haskellPackages." <> T.unpack pkg
    result <- try (readProcess "nix-env" ["-f", "<nixpkgs>", "-qaP", "-A", searchTerm] "") :: IO (Either SomeException String)
    case result of
        Right output -> return (not $ null output)
        Left ex -> do
            return False



-- Function to convert a list of Texts to a comma-separated string
commaSeparatedList :: [T.Text] -> T.Text
commaSeparatedList = T.intercalate ", "

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> do
            content <- TIO.readFile filePath
            let modules = extractModuleNames content
                packages = map moduleToPackage modules
                nonBasePackages = filter (/= "base") packages            
            availablePackages <- filterM searchNixPackage packages
            if null availablePackages
                then putStrLn "No matching NixOS packages found for the imported modules."
                else TIO.putStrLn $ "Available NixOS packages: " <> commaSeparatedList availablePackages
        _ -> putStrLn "Usage: runhaskell SearchNixPackages.hs <HaskellSourceFile.hs>"
