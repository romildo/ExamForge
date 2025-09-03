module Main where

import Generated.Questions (questionPool)
import ExamForge.Exam
import ExamForge.Type (SelectionType(..))
import qualified ExamForge.Formatter.Latex as LatexFormatter
import ExamForge.Formatter.Latex (FormatterConfig(..))

import System.Random (StdGen, newStdGen, split, randomR)
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)
import Text.Printf (printf)
import Data.List (transpose, unfoldr, mapAccumL, intercalate)
import Data.Maybe (fromMaybe)
import Text.Regex.TDFA ((=~))
import Text.Show.Pretty (pPrint)

-- =============================================================================
-- == Configuration and Argument Parsing
-- =============================================================================

data Filter = Filter
  { fId      :: Maybe String
  , fTitle   :: Maybe String
  , fSubject :: Maybe String
  , fTag     :: Maybe String
  } deriving (Show)

data Config = Config
  { cFilters      :: Filter
  , cVersions     :: Int
  , cOutDir       :: FilePath
  , cCsvFile      :: Maybe FilePath
  , cLatexConfig  :: FormatterConfig
  } deriving (Show)

defaultConfig :: Config
defaultConfig = Config
  { cFilters      = Filter Nothing Nothing Nothing Nothing
  , cVersions     = 1
  , cOutDir       = "exams"
  , cCsvFile      = Nothing
  , cLatexConfig  = FormatterConfig { showId = False, showTags = False, showSubject = True }
  }

parseArgs :: [String] -> IO Config
parseArgs args = go args defaultConfig
  where
    go [] conf = return conf
    go ("--id":r:rest) conf         = go rest $ conf { cFilters = (cFilters conf) { fId = Just r } }
    go ("--title":r:rest) conf      = go rest $ conf { cFilters = (cFilters conf) { fTitle = Just r } }
    go ("--subject":r:rest) conf    = go rest $ conf { cFilters = (cFilters conf) { fSubject = Just r } }
    go ("--tag":r:rest) conf        = go rest $ conf { cFilters = (cFilters conf) { fTag = Just r } }
    go ("--versions":n:rest) conf
      | all (`elem` "0123456789") n = go rest $ conf { cVersions = read n }
    go ("--out-dir":d:rest) conf    = go rest $ conf { cOutDir = d }
    go ("--csv-file":f:rest) conf   = go rest $ conf { cCsvFile = Just f }
    go ("--show-id":rest) conf      = go rest $ conf { cLatexConfig = (cLatexConfig conf) { showId = True } }
    go ("--show-tags":rest) conf    = go rest $ conf { cLatexConfig = (cLatexConfig conf) { showTags = True } }
    go ("--hide-subject":rest) conf = go rest $ conf { cLatexConfig = (cLatexConfig conf) { showSubject = False } }
    go ("--help":_) _               = usage >> exitSuccess
    go (x:_) _                     = putStrLn ("Unknown option: " ++ x) >> usage >> exitFailure

usage :: IO ()
usage = putStrLn $ unlines
  [ "Usage: exam-assembler [OPTIONS]"
  , ""
  , "Selection and Output:"
  , "  --versions N     Number of exam versions to generate (default: 1)."
  , "  --out-dir DIR    Directory to save final .tex files (default: \"exams\")."
  , "  --csv-file FILE  File path for the CSV answer key output."
  , ""
  , "Filtering Options:"
  , "  --id REGEX       Filter questions by ID."
  , "  --title REGEX    Filter questions by title."
  , "  --subject REGEX  Filter questions by subject."
  , "  --tag REGEX      Filter questions by tag."
  , ""
  , "Formatting Options:"
  , "  --show-id        Include question ID in the LaTeX output."
  , "  --show-tags      Include question tags in the LaTeX output."
  , "  --hide-subject   Do not include the question subject (shown by default)."
  , "  --help           Show this help message."
  ]

-- =============================================================================
-- == Core Logic
-- =============================================================================

applyFilters :: Filter -> [Question] -> [Question]
applyFilters filters =
    map (applyId . applyTitle . applySubject . applyTag)
  where
    applyMaybeFilter :: (Question -> Maybe String) -> Maybe String -> Question -> Question
    applyMaybeFilter field (Just regex) q = if fromMaybe False ((=~ regex) <$> field q) then q else q { qVariants = [] }
    applyMaybeFilter _ Nothing q = q

    applyTag     = applyMaybeFilter (\q -> Just (unwords (qTags q))) (fTag filters)
    applySubject = applyMaybeFilter qSubject (fSubject filters)
    applyTitle   = applyMaybeFilter (Just . qTitle) (fTitle filters)
    applyId      = applyMaybeFilter (Just . qId) (fId filters)

shuffle :: StdGen -> [a] -> ([a], StdGen)
shuffle gen [] = ([], gen)
shuffle gen xs = (e : fst (shuffle g' (left ++ right)), g')
  where
    (i, g') = randomR (0, length xs - 1) gen
    (left, e:right) = splitAt i xs

cycleShuffle :: StdGen -> [a] -> [a]
cycleShuffle _ [] = []
cycleShuffle gen xs =
  let (xs', gen') = shuffle gen xs
  in xs' ++ cycleShuffle gen' xs

shufflePair :: StdGen -> (a, [b]) -> (StdGen, (a, [b]))
shufflePair g (a, bList) =
  let (bList', newG) = shuffle g bList
  in (newG, (a, bList'))

writeCsvFile :: FilePath -> [(Int, [String])] -> IO ()
writeCsvFile path examAnswers = do
  let
    numQuestions = case examAnswers of
                     ((_, answers):_) -> length answers
                     [] -> 0
    header = "Prova_Tipo," ++ intercalate "," [ "Q" ++ show n | n <- [1..numQuestions] ]
    formatRow (version, answers) = printf "B-%02d" version ++ "," ++ intercalate "," answers
    content = unlines $ header : map formatRow examAnswers
  writeFile path content
  putStrLn $ "CSV answer key saved to: " ++ path

-- =============================================================================
-- == Main Orchestration
-- =============================================================================

main :: IO ()
main = do
  config <- getArgs >>= parseArgs
  let numVersions = cVersions config
  let outDir = cOutDir config
  let latexConfig = cLatexConfig config

  putStrLn $ "--- ExamForge Assembler ---"
  putStrLn $ "Generating " ++ show numVersions ++ " exam version(s)..."
  putStrLn $ "Output directory: " ++ outDir

  let transformedQuestions = applyFilters (cFilters config) questionPool
  let filteredQuestions = filter (not . null . qVariants) transformedQuestions

  if null filteredQuestions
    then putStrLn "No questions matched the filter criteria. Aborting." >> exitFailure
    else putStrLn $ "Found " ++ show (length filteredQuestions) ++ " matching questions."

  initialGen <- newStdGen
  let (genForStreams, genForShuffling) = split initialGen
  let gens = unfoldr (Just . split) genForStreams
  let allVariants = map qVariants filteredQuestions
  let infiniteVariantStreams = zipWith cycleShuffle gens allVariants
  let streamOfExams = transpose infiniteVariantStreams
  let examsToBuild = take numVersions streamOfExams

  createDirectoryIfMissing True outDir

  let (_, processedExams) = mapAccumL (shuffleAndFormat filteredQuestions latexConfig) genForShuffling (zip [1..] examsToBuild)

  mapM_ (\(version, tex, _) -> writeFile (outDir </> printf "exam-B%02d.tex" version) tex) processedExams
  putStrLn "\nExam .tex generation complete."

  case cCsvFile config of
    Nothing -> return ()
    Just csvPath -> do
      let allAnswerKeys = [(v, key) | (v, _, key) <- processedExams]
      writeCsvFile csvPath allAnswerKeys

shuffleAndFormat :: [Question] -> FormatterConfig -> StdGen -> (Int, [Variant]) -> (StdGen, (Int, String, [String]))
shuffleAndFormat questions latexConfig gen (version, variants) =
  let
    (gen', variantsWithShuffledAnswers) = mapAccumL shufflePair gen variants
    
    examData = zip questions variantsWithShuffledAnswers
    (shuffledExamData, gen'') = shuffle gen' examData

    answerKey = map (uncurry LatexFormatter.formatCorrectAnswers) shuffledExamData

    latexOutput = LatexFormatter.format latexConfig version shuffledExamData
  in
    (gen'', (version, latexOutput, answerKey))
