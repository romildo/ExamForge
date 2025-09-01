module Main where

import ExamForge.Parser ()
import ExamForge.Type (QuestionTemplate)
import ExamForge.Generator (generateQuestionsModule)

import Data.Yaml (decodeFileEither)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import Text.Printf (printf)
import Control.Monad (foldM)
import Data.List (intercalate, isPrefixOf) -- Import isPrefixOf

data Config = Config
  { inputFiles    :: [FilePath]
  , outputBaseDir :: FilePath
  }

generatedModuleName :: String
generatedModuleName = "Generated.Questions"

-- A simple helper to split a string by a delimiter, using standard String.
splitOn :: String -> String -> [String]
splitOn [] str = map (:[]) str -- Edge case: split by empty string
splitOn delim str
  | null rest = [chunk]
  | otherwise = chunk : splitOn delim (drop (length delim) rest)
  where
    go [] = ("", "")
    go s@(c:cs)
      | delim `isPrefixOf` s = ("", s)
      | otherwise            = let (recChunk, recRest) = go cs in (c:recChunk, recRest)
    (chunk, rest) = go str

-- Converts a module name to its corresponding file path using standard String functions.
moduleNameToPath :: String -> FilePath
moduleNameToPath modName = intercalate "/" (splitOn "." modName) ++ ".hs"

parseArgs :: [String] -> IO Config
parseArgs args = go args defaultConfig
  where
    defaultConfig = Config { inputFiles = [], outputBaseDir = "generated" }
    go [] conf = return conf
    go ("--output":o:rest) conf = go rest $ conf { outputBaseDir = o }
    go ("--help":_) _ = usage >> exitSuccess
    go (f:rest) conf = go rest $ conf { inputFiles = inputFiles conf ++ [f] }

usage :: IO ()
usage = putStrLn $ unlines
  [ "Usage: examforge [OPTIONS] [INPUT_FILES...]"
  , ""
  , "  INPUT_FILES...   One or more .yml files containing question templates."
  , ""
  , "Options:"
  , "  --output DIR     Base directory for the generated Haskell module (default: \"generated\")."
  , "  --help           Show this help message."
  ]

main :: IO ()
main = do
  args <- getArgs
  config <- parseArgs args

  if null (inputFiles config)
    then putStrLn "No input files provided. Use --help for usage." >> exitFailure
    else do
      let finalOutputFile = outputBaseDir config </> moduleNameToPath generatedModuleName
      
      putStrLn "--- ExamForge Code Generator ---"
      putStrLn $ "Output file: " ++ finalOutputFile

      allTemplates <- foldM processFile [] (inputFiles config)
      putStrLn $ "\nRead " ++ show (length allTemplates) ++ " question templates in total."

      let generatedCode = generateQuestionsModule generatedModuleName allTemplates

      createDirectoryIfMissing True (takeDirectory finalOutputFile)
      writeFile finalOutputFile generatedCode

      putStrLn $ "Successfully generated module: " ++ generatedModuleName
      putStrLn $ "Saved to: " ++ finalOutputFile

processFile :: [QuestionTemplate] -> FilePath -> IO [QuestionTemplate]
processFile acc filepath = do
  printf "Processing: %s\n" filepath
  parseResult <- decodeFileEither filepath
  case parseResult of
    Left err -> do
      putStrLn $ "  [ERROR] Failed to parse: " ++ show err
      return acc
    Right templates -> do
      putStrLn $ "  [OK] Found " ++ show (length templates) ++ " question templates."
      return (acc ++ templates)
