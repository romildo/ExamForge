module Main where

import Generated.Questions (questionPool)
import ExamForge.Exam
import ExamForge.Type (SelectionType(..), QuestionTemplate(..))
import qualified ExamForge.Formatter.Latex as LatexFormatter
import ExamForge.Formatter.Latex (FormatterConfig(..))
import ExamConfig

import System.Random (StdGen, newStdGen, split, randomR)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeBaseName) -- Import takeBaseName
import System.Directory (createDirectoryIfMissing)
import Text.Printf (printf)
import Data.List (transpose, unfoldr, mapAccumL, intercalate)
import Data.Maybe (fromMaybe)
import Control.Monad (unless)
import Text.Regex.TDFA ((=~))
import Text.Show.Pretty (pPrint)

main :: IO ()
main = do
  args <- getArgs
  unless (length args == 1) $ do
    putStrLn "Usage: exam-assembler <path-to-exam-config.yml>"
    exitFailure
  let configFile = head args

  -- Extract the basename from the config file path
  let baseName = takeBaseName configFile

  configResult <- loadConfig configFile
  case configResult of
    Left err -> print err >> exitFailure
    Right config -> do
      let filteredQuestions = filterQuestions (selection config) questionPool
      putStrLn $ "Found " ++ show (length filteredQuestions) ++ " matching questions."
      -- Pass the basename to the assembly function
      assembleExams baseName config filteredQuestions

-- | An explicit type signature for the helper function to resolve ambiguity.
matchesAny :: [String] -> [String] -> Bool
matchesAny patterns tags = any (\tag -> any (tag =~) patterns) tags

filterQuestions :: Selection -> [Question] -> [Question]
filterQuestions sel = filter (passesAllFilters sel)
  where
    passesAllFilters s q =
      let inclPatterns = include_tags s
          exclPatterns = exclude_tags s
          questionTags = qTags q
          
          isIncluded = null inclPatterns || matchesAny inclPatterns questionTags
          isExcluded = not (null exclPatterns) && matchesAny exclPatterns questionTags

      in isIncluded && not isExcluded

assembleExams :: String -> Config -> [Question] -> IO ()
assembleExams baseName config filteredQuestions
  | null filteredQuestions = putStrLn "No questions matched the filters. No exams generated."
  | otherwise = do
      let numVersions = versions (assembly_options config)
      let outDir = "exams"

      let latexConfig = FormatterConfig
            { showId = show_id (assembly_options config)
            , showTags = show_tags (assembly_options config)
            , showSubject = not (hide_subjects (assembly_options config))
            }

      initialGen <- newStdGen
      let (genForStreams, genForShuffling) = split initialGen
      let gens = unfoldr (Just . split) genForStreams
      let allVariants = map qVariants filteredQuestions
      let infiniteVariantStreams = zipWith cycleShuffle gens allVariants
      let streamOfExams = transpose infiniteVariantStreams
      let examsToBuild = take numVersions streamOfExams

      createDirectoryIfMissing True outDir

      let (_, processedExams) = mapAccumL (shuffleAndFormat (header config) filteredQuestions latexConfig) genForShuffling (zip [1..] examsToBuild)

      -- NEW: Use the baseName for the output .tex files
      mapM_ (\(version, tex, _) -> writeFile (outDir </> printf "%s-%02d.tex" baseName version) tex) processedExams
      putStrLn "\nExam .tex generation complete."
      
      let allAnswerKeys = [(v, key) | (v, _, key) <- processedExams]
      -- NEW: Use the baseName for the output .csv file
      writeCsvFile (outDir </> printf "%s.keys.csv" baseName) allAnswerKeys

shuffleAndFormat :: Header -> [Question] -> FormatterConfig -> StdGen -> (Int, [Variant]) -> (StdGen, (Int, String, [String]))
shuffleAndFormat header questions latexConfig gen (version, variants) =
  let
    (gen', variantsWithShuffledAnswers) = mapAccumL shufflePair gen variants
    examData = zip questions variantsWithShuffledAnswers
    (shuffledExamData, gen'') = shuffle gen' examData
    answerKey = map (uncurry LatexFormatter.formatCorrectAnswers) shuffledExamData
    latexOutput = LatexFormatter.format header latexConfig version shuffledExamData
  in
    (gen'', (version, latexOutput, answerKey))

-- Corrected shuffling functions from your original file
shuffle :: StdGen -> [a] -> ([a], StdGen)
shuffle gen [] = ([], gen)
shuffle gen l =
  let
    (i, gen') = randomR (0, length l - 1) gen
    (xs, y:ys) = splitAt i l
    (shuffledRest, finalGen) = shuffle gen' (xs ++ ys)
  in
    (y : shuffledRest, finalGen)

cycleShuffle :: StdGen -> [a] -> [a]
cycleShuffle _ [] = []
cycleShuffle gen xs =
  let (shuffled, gen') = shuffle gen xs
  in shuffled ++ cycleShuffle gen' xs

shufflePair :: StdGen -> (a, [b]) -> (StdGen, (a, [b]))
shufflePair gen (text, answers) =
  let (shuffledAnswers, gen') = shuffle gen answers
  in (gen', (text, shuffledAnswers))

writeCsvFile :: FilePath -> [(Int, [String])] -> IO ()
writeCsvFile _ [] = putStrLn "Warning: No data to write to CSV."
writeCsvFile path dataRows = do
    -- The CSV header can be generic
    let header = "Exam_Type," ++ intercalate "," (map (\i -> "Q" ++ show i) [1..length (snd (head dataRows))])
    let rows = map (\(v, ans) -> show v ++ "," ++ intercalate "," ans) dataRows
    writeFile path (unlines (header:rows))
    putStrLn $ "Answer key written to " ++ path