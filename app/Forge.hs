module Main where

import ExamForge.Type (QuestionTemplate)
import ExamForge.Parser () -- <--- ADDED THIS LINE to bring the instance into scope
import ExamForge.Generator (generateQuestionsModule)
import ExamConfig (loadConfig, question_banks)
import System.FilePath.Glob (glob)

import Data.Yaml (decodeFileEither, ParseException, prettyPrintParseException)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Printf (printf)
import Control.Monad (foldM, unless)
import Data.List (intercalate, isPrefixOf)
import Text.Show.Pretty (pPrint)

generatedModuleName :: String
generatedModuleName = "Generated.Questions"

-- Helper function from your code
splitOn :: String -> String -> [String]
splitOn [] str = map (:[]) str
splitOn delim str
  | null rest = [chunk]
  | otherwise = chunk : splitOn delim (drop (length delim) rest)
  where
    go [] = ("", "")
    go s@(c:cs)
      | delim `isPrefixOf` s = ("", s)
      | otherwise            = let (recChunk, recRest) = go cs in (c:recChunk, recRest)
    (chunk, rest) = go str

-- Helper function from your code
moduleNameToPath :: String -> FilePath
moduleNameToPath modName = intercalate "/" (splitOn "." modName) ++ ".hs"

main :: IO ()
main = do
  -- 1. Get the single config file path from arguments
  args <- getArgs
  unless (length args == 1) $ do
    putStrLn "Usage: examforge <path-to-exam-config.yml>"
    exitFailure
  let configFile = head args

  -- 2. Load and parse the YAML config
  configResult <- loadConfig configFile
  putStrLn "=== config:" >> pPrint configResult
  case configResult of
    Left err -> do
      putStrLn $ "Error parsing configuration file: " ++ prettyPrintParseException err
      exitFailure
    Right config -> do
      -- 3. Expand glob patterns to get the list of question bank files
      let patterns = question_banks config
      questionFiles <- concat <$> mapM glob patterns

      putStrLn "--- ExamForge Code Generator ---"
      putStrLn $ "Processing " ++ show (length questionFiles) ++ " question bank(s):"
      mapM_ (putStrLn . ("  - " ++)) questionFiles

      -- 4. Process all found files
      allTemplates <- foldM processFile [] questionFiles
      putStrLn $ "\nRead " ++ show (length allTemplates) ++ " question templates in total."

      -- 5. Generate and write the Haskell module
      let outputBaseDir = "generated"
      let finalOutputFile = outputBaseDir </> moduleNameToPath generatedModuleName
      let generatedCode = generateQuestionsModule generatedModuleName allTemplates

      createDirectoryIfMissing True (takeDirectory finalOutputFile)
      writeFile finalOutputFile generatedCode

      putStrLn $ "Successfully generated module: " ++ generatedModuleName
      putStrLn $ "Saved to: " ++ finalOutputFile

-- Your robust file processing function, using decodeFileEither
processFile :: [QuestionTemplate] -> FilePath -> IO [QuestionTemplate]
processFile acc filepath = do
  parseResult <- decodeFileEither filepath :: IO (Either ParseException [QuestionTemplate])
  case parseResult of
    Left err -> do
      printf "  [ERROR] Failed to parse %s: %s\n" filepath (prettyPrintParseException err)
      return acc
    Right templates -> do
      printf "  [OK] Found %d question templates in %s.\n" (length templates) filepath
      return (acc ++ templates)