{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Monad (forM)
import Control.Exception (throwIO)
import System.Exit (die)
import System.FilePath (takeBaseName)
import Text.Pretty.Simple (pShow)
import Text.Pretty.Simple (pShow)
import Data.Text.Lazy (unpack)
import System.IO (hSetEncoding, stdout, stderr, utf8)
import Data.Aeson (FromJSON, ToJSON, encode)
import Data.Yaml (decodeFileEither, ParseException(..), YamlException(..), YamlMark(..))
import qualified Data.ByteString.Lazy.Char8 as BSL

import ExamForge.ExamConfig (ExamConfig(..), EvaluatorConfig(..))
import ExamForge.QuestionBank (QuestionTemplate(..), Answer(..), Delimiters(..))
import ExamForge.Template (parseTemplate, AnswerAST(..))
import ExamForge.Evaluator.Class (generateScript)
import ExamForge.Evaluator.Python (PythonEval(..))
import ExamForge.Evaluator.C (CEval(..))
import ExamForge.Evaluator.Cpp (CppEval(..))
import ExamForge.Evaluator.Haskell (HaskellEval(..))
import ExamForge.Orchestrator (evaluateQuestion)
import ExamForge.IR (toCoreQuestion)
import ExamForge.Exam (Question)
import ExamForge.Mock (MockOpts(..), mockParser, runMock)

-- IMPORT YOUR EXISTING ASSEMBLY AND LATEX MODULES HERE
import ExamForge.Assembler (filterQuestions, generateExamQuestionLists, assembleExams)
-- import ExamForge.Formatter.Latex (generateLatexFiles)

-- | 1. Built-in defaults
defaultEvaluators :: Map String EvaluatorConfig
defaultEvaluators = Map.fromList
  [ ("python",  EvaluatorConfig Nothing "python3 %f" (Just "py"))
  , ("c",       EvaluatorConfig (Just "gcc -Wall -o %e %f") "%e" (Just "c"))
  , ("cpp",     EvaluatorConfig (Just "g++ -Wall -std=c++17 -o %e %f") "%e" (Just "cpp")) -- C++ with c++17 support
  , ("haskell", EvaluatorConfig Nothing "runhaskell %f" (Just "hs"))
  ]

-- | 2. CLI Options
data Command
  = Build BuildOpts
  | Check CheckOpts
  | Mock MockOpts

newtype BuildOpts = BuildOpts { buildConfigFile :: FilePath }

data CheckOpts = CheckOpts
  { checkBankFile   :: FilePath
  , checkConfigFile :: Maybe FilePath
  , checkQuestionId :: Maybe String
  , checkShowScript :: Bool
  , checkShowJson   :: Bool
  }

-- | 3. CLI Parsers
buildParser :: Parser Command
buildParser = Build <$> (BuildOpts <$> strArgument (metavar "CONFIG_FILE" <> help "Path to exam-config.yml"))

checkParser :: Parser Command
checkParser = Check <$> (CheckOpts
  <$> strArgument (metavar "BANK_FILE" <> help "Path to the question bank YAML")
  <*> optional (strOption (long "config" <> short 'c' <> metavar "CONFIG_FILE" <> help "Optional exam config override"))
  <*> optional (strOption (long "id" <> short 'i' <> metavar "QUESTION_ID" <> help "Only check a specific question ID"))
  <*> switch (long "show-script" <> short 's' <> help "Output the generated script instead of evaluating it")
  <*> switch (long "show-json" <> short 'j' <> help "Output the evaluated JSON IR"))

commandParser :: Parser Command
commandParser = subparser
  ( command "build" (info buildParser (progDesc "Assemble a complete exam from a config file"))
 <> command "check" (info checkParser (progDesc "Dry-run evaluate a question bank"))
 <> command "mock"  (info (Mock <$> mockParser) (progDesc "Generate mock banks and configs for load testing"))
  )

-- | 4. The Polyglot Router
-- | Purely generates the raw script for a question template
generateTemplateScript :: String -> QuestionTemplate -> Either String String
generateTemplateScript defaultLang qt =
  let lang = fromMaybe defaultLang (qtLanguage qt)
      delims = fromMaybe (Delimiters "{{" "}}") (qtDelimiters qt)
      parseAST str = parseTemplate (startDelim delims, endDelim delims) str
      
      ansASTs = map (\ans -> case ans of
                       CorrectAnswer t   -> AnswerAST True (parseAST t)
                       IncorrectAnswer t -> AnswerAST False (parseAST t)
                    ) (qtAnswers qt)
  -- Route to the correct language module
  in case lang of
       "python"  -> Right $ generateScript PythonEval qt (parseAST (qtQuestion qt)) ansASTs
       "c"       -> Right $ generateScript CEval qt (parseAST (qtQuestion qt)) ansASTs
       "cpp"     -> Right $ generateScript CppEval qt (parseAST (qtQuestion qt)) ansASTs
       "haskell" -> Right $ generateScript HaskellEval qt (parseAST (qtQuestion qt)) ansASTs
       _         -> Left $ "Unsupported language module: " ++ lang

-- | Evaluates a question template by generating the script and sending it to the Orchestrator
processTemplate :: Map String EvaluatorConfig -> String -> QuestionTemplate -> IO (Either String Question)
processTemplate evaluators defaultLang qt = do
  case generateTemplateScript defaultLang qt of
    Left err -> return (Left err)
    Right script -> do
      let lang = fromMaybe defaultLang (qtLanguage qt)
      let evalCfg = Map.findWithDefault (EvaluatorConfig Nothing (lang ++ " %f") Nothing) lang evaluators
      
      res <- evaluateQuestion evalCfg script
      return $ fmap toCoreQuestion res

-- | 5. Execution Handlers
runBuild :: BuildOpts -> IO ()
runBuild opts = do
  putStrLn $ "Loading config: " ++ buildConfigFile opts
  cfg <- loadYamlOrDie "exam config" (buildConfigFile opts)

  let activeEvaluators = Map.union (evaluators cfg) defaultEvaluators
  let defLang = fromMaybe "python" (default_language cfg)

  putStrLn "Loading question banks..."
  -- Load all templates from all files (you might need to expand globs here if you use them)
  -- Map the helper over all configured banks and flatten the result
  nestedTemplates <- mapM (loadYamlOrDie "Question bank") (question_banks cfg)
  let allTemplates = concat nestedTemplates  

  putStrLn $ "Evaluating " ++ show (length allTemplates) ++ " templates..."
  evalResults <- mapM (processTemplate activeEvaluators defLang) allTemplates

  -- Separate successes from failures
  let (errors, coreQuestions) = foldr (\res (es, qs) -> case res of 
                                        Left e -> (e:es, qs)
                                        Right q -> (es, q:qs)) ([], []) evalResults

  if not (null errors)
    then die $ "Evaluation failed with errors:\n" ++ unlines errors
    else do
      putStrLn $ "Successfully evaluated " ++ show (length coreQuestions) ++ " questions."
      putStrLn "Assembling exam..."
      
      -- TODO: Plug your existing Assembly logic here!
      let filteredPool = filterQuestions (selection cfg) coreQuestions
      -- Extract the basename from the config file path
      let baseName = takeBaseName (buildConfigFile opts)
      -- Pass the basename to the assembly function
      assembleExams baseName cfg filteredPool

      -- let examVersions = generateExamQuestionLists ... filteredPool
      -- generateLatexFiles ... examVersions
      
      putStrLn "Done!"

runCheck :: CheckOpts -> IO ()
runCheck opts = do
  putStrLn $ "Checking bank: " ++ checkBankFile opts
  templates <- loadYamlOrDie "question bank" (checkBankFile opts)
  
  -- Filter by ID if requested
  let targetTemplates = case checkQuestionId opts of
        Nothing  -> templates
        Just qid -> filter (\t -> qtId t == qid) templates

  if checkShowScript opts
    then do
      putStrLn $ "Dumping scripts for " ++ show (length targetTemplates) ++ " templates...\n"
      mapM_ (\qt -> do
          putStrLn $ "# =========================================="
          putStrLn $ "# SCRIPT FOR: " ++ qtId qt
          putStrLn $ "# ==========================================\n"
          case generateTemplateScript "python" qt of
            Left err     -> putStrLn $ "ERROR: " ++ err
            Right script -> putStrLn script
            
          putStrLn "\n"
        ) targetTemplates
    else do
      putStrLn $ "Found " ++ show (length targetTemplates) ++ " matching templates. Evaluating...\n"
      evalResults <- mapM (processTemplate defaultEvaluators "python") targetTemplates

      mapM_ (\res -> case res of
                Left err -> putStrLn $ "[ERROR]\n" ++ err
                Right q  -> do
                    putStrLn "[SUCCESS]"
                    if checkShowJson opts
                      then BSL.putStrLn (encode q)
                      else putStrLn $ unpack (pShow q)
            ) evalResults

-- | Load a YAML file and handle parse errors with the file name
loadYamlOrDie :: FromJSON a => String -> FilePath -> IO a
loadYamlOrDie description filepath = do
  res <- decodeFileEither filepath
  case res of
    Right result -> return result
    Left err -> case err of
      InvalidYaml (Just (YamlParseException {yamlProblemMark=mark, yamlProblem=problem, yamlContext=context})) ->
        die $ "YAML parse exception at " ++
                   filepath ++ 
                   ":" ++ show (yamlLine mark + 1) ++
                   ":" ++ show (yamlColumn mark + 1) ++ 
                   "\n" ++ context ++ ":" ++
                   "\n" ++ problem
      _ -> throwIO err

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  let opts = info (commandParser <**> helper)
        ( fullDesc <> progDesc "ExamForge v4.0 - Polyglot Exam Generation System" )
  cmd <- execParser opts
  case cmd of
    Build bOpts -> runBuild bOpts
    Check cOpts -> runCheck cOpts
    Mock mOpts  -> runMock mOpts
