-- File: app/Assemble.hs 
module Main where

import Generated.Questions (questionPool)
import ExamForge.Exam
import ExamForge.Type (SelectionType(..), QuestionTemplate(..))
import qualified ExamForge.Formatter.Latex as LatexFormatter
import ExamForge.Formatter.Latex (FormatterConfig(..))
import ExamForge.ExamConfig

import System.Random (StdGen, newStdGen, mkStdGen, split, randomR)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeBaseName)
import System.Directory (createDirectoryIfMissing)
import Text.Printf (printf)
import Data.List (transpose, unfoldr, mapAccumL, intercalate, partition, sort, nub, delete, deleteBy)
import Data.Maybe (fromMaybe, isJust)
import Control.Monad (unless, forM)
import Text.Regex.TDFA ((=~))
import qualified Data.Map.Strict as Map
import Debug.Trace (traceShowWith)
import Debug.Pretty.Simple (pTraceShowWith, pTraceWith, pTrace, pTraceShow)

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
      putStrLn $ "Found " ++ show (length filteredQuestions) ++ " matching questions after tag filtering."
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

-- | Single list rotation (left shift): This moves the first element to the end of the list
rotateOnce :: [a] -> [a]
rotateOnce [] = []
rotateOnce (x:xs) = xs ++ [x]

-- | Simplify pool ommiting irrelevant details when printing debug messages
simplifyPool :: ((String, Int, [Question]), Int, [Question], [Question]) -> (String, Int, [String], [String])
simplifyPool ((t, n, qs), n', qs', stream) =
  (t, n', map qId qs', map qId $ take (2*n) stream)

-- | Check if two questions have the same ID
sameId :: Question -> Question -> Bool
sameId q1 q2 = qId q1 == qId q2

-- | Pure function to generate an infinite list of question subsets per exam version,
-- strictly respecting semantic group quotas and rotating selections.
generateExamQuestionLists :: StdGen -> [SemanticGroupRule] -> Bool -> [Question] -> [[Question]]
generateExamQuestionLists _ [] _ questions = repeat questions -- No constraints, use all questions every time
generateExamQuestionLists gen rules shouldShuffleQs questions =
  let
    -- Evaluates rules top-to-bottom. Returns Just (quota) on first match.
    getTagQuota :: String -> Maybe Int
    getTagQuota tag = 
      case filter (\rule -> tag =~ tag_pattern rule :: Bool) rules of
        (firstMatch:_) -> Just (maximum_qs firstMatch)
        []             -> Nothing

    isSemantic :: String -> Bool
    isSemantic tag = isJust (getTagQuota tag)
                       
    getSemanticTags q = filter isSemantic (qTags q)
    
    (constrained, unconstrained) = partition (not . null . getSemanticTags) questions
    
    semanticTags = filter isSemantic (nub (concatMap qTags questions))

    -- | Constructs the initial state tuple for a single semantic group.
    -- The tuple structure is: (StaticInfo, CurrentQuota, RemainingQuestions, InfiniteStream)
    --
    -- 1. StaticInfo: (tag, max_quota, all_group_questions)
    --    - 'tag': The semantic group name (e.g., "grupo-A").
    --    - 'max_quota': The maximum number of questions that can be selected per exam (min of maxPer and total questions).
    --    - 'all_group_questions': The immutable list of all questions belonging to this tag.
    --
    -- 2. CurrentQuota: The number of questions left to pick for the *current* exam variant.
    --
    -- 3. RemainingQuestions: The pool of questions that have NOT yet been picked for the *current* exam variant.
    --
    -- 4. InfiniteStream: The continuously rotating (and optionally shuffled) stream of all questions in this group.
    mkSemanticGroup tag =
      let groupQuestions = filter (elem tag . qTags) questions
          -- Safely extract the quota (we know it's Just because 'tag' is in semanticTags)
          Just maxPer = getTagQuota tag 
          n = min (length groupQuestions) maxPer
          cycleList | shouldShuffleQs = cycleShuffle gen
                    | otherwise = cycle
      in ( (tag, n, groupQuestions) 
         , n                        
         , groupQuestions           
         , cycleList groupQuestions 
         )

    semanticGroups = map mkSemanticGroup semanticTags

    selectQuestions selected usedPool [] =
      (selected, usedPool)

    selectQuestions selected usedPool (x@(_, n, qs, _) : rest)
      | n <= 0 || null qs =
        selectQuestions selected (x : usedPool) rest

    selectQuestions selected usedPool pool@((t, n, qs, stream) : _) =
      let 
        (_, q:_) = span (\x -> not (any (sameId x) qs)) stream
        
        -- Step 1: Standard update - decrement quotas and remove the picked question
        updateGroupInfo qPicked groupInfo@((tag, maxQ, allQs), currN, currQs, currStream)
          | any (sameId qPicked) currQs = 
              ((tag, maxQ, allQs), currN - 1, deleteBy sameId qPicked currQs, deleteBy sameId qPicked currStream)
          | otherwise = groupInfo  
          
        usedPool1 = map (updateGroupInfo q) usedPool
        pool1     = map (updateGroupInfo q) pool

        -- Step 2: Identify any groups that have just hit their quota limit (n <= 0)
        exhaustedQs = nub $ concatMap (\(_, n, qss, _) -> if n <= 0 then qss else []) (usedPool1 ++ pool1)

        -- Step 3: Remove those exhausted questions from the available pools of all OTHER groups
        pruneExhausted groupInfo@((tag, maxQ, allQs), currN, currQs, currStream) =
             ((tag, maxQ, allQs), currN, filter (\x -> not (any (sameId x) exhaustedQs)) currQs, currStream)

        usedPool' = map pruneExhausted usedPool1
        pool'     = map pruneExhausted pool1

      in selectQuestions (q:selected) usedPool' (rotateOnce pool')


    resetSemanticGroup (x@(t, n, questions), _, _, stream) = (x, n, questions, stream)

    resetPool pool = map resetSemanticGroup pool

    generateAllExams pool = exam : generateAllExams (resetPool finalPool)
      where
        (selected, finalPool) = selectQuestions [] [] pool
        exam = unconstrained ++ selected -- Order will be shuffled later if requested
    
  in generateAllExams semanticGroups

-- | Map Question IDs to their infinite, cycled-shuffled stream of variants
type VariantMap = Map.Map String [Variant]

assembleExams :: String -> Config -> [Question] -> IO ()
assembleExams baseName config filteredQuestions
  | null filteredQuestions = putStrLn "No questions matched the filters. No exams generated."
  | otherwise = do
      let numVersions = versions (assembly_options config)
      let shouldShuffleQs = shuffle_questions (assembly_options config)
      let outDir = "exams"

      let latexConfig = FormatterConfig
            { showId = show_id (assembly_options config)
            , showTags = show_tags (assembly_options config)
            , showSubject = not (hide_subjects (assembly_options config))
            }

      -- Initialize Pseudo-Random Generators
      let seed = 20252
      let initialGen = mkStdGen seed
      let (genGroups, genRest) = split initialGen
      let (genVariants, genShuffle) = split genRest

      -- 1. Determine which questions go into which version (Semantic Groups)
      let infiniteExamLists = generateExamQuestionLists genGroups (semantic_groups $ selection config) shouldShuffleQs filteredQuestions
      let examsToBuild = take numVersions infiniteExamLists

      -- Debug: print the exams questions
      putStrLn "Filtered questions:"
      forM (zip [1::Integer ..] filteredQuestions) $ \(i, q) ->
        putStrLn (printf "  %2d. %s: %s" i (qId q) (intercalate "," (qTags q)))
      putStrLn ""
      forM (zip [1::Integer ..] examsToBuild) $ \(counter, exam) ->
        do
          putStrLn (printf "Exam %d:" counter)
          forM (zip [1::Integer ..] (sort exam)) $ \(qcounter, q) ->
            putStrLn (printf "        %2d. %s: %s" qcounter (qId q) (intercalate "," (qTags q)))
      
      -- 2. Initialize Variant Streams for every potential question
      let gensForVariants = unfoldr (Just . split) genVariants
      let initialVariantMap = Map.fromList 
            [ (qId q, cycleShuffle g (qVariants q)) 
            | (q, g) <- zip filteredQuestions gensForVariants 
            ]

      createDirectoryIfMissing True outDir

      -- 3. Assemble the exams by threading the VariantMap state
      let buildVersion (vMap, currentGen) (vNum, qs) =
            let
              -- Shuffle the order of questions in the exam if configured
              (shuffledQs, gen1) = if shouldShuffleQs then shuffle currentGen qs else (qs, currentGen)
              
              -- Extract next variant for each selected question
              -- Note the accumulator (vm, g) is the FIRST argument to match mapAccumL
              extractVariant (vm, g) q =
                let (v:vs) = vm Map.! qId q
                    -- shufflePair returns (StdGen, Variant)
                    (g', shuffledAnswers) = shufflePair g v
                    vm' = Map.insert (qId q) vs vm
                in ((vm', g'), (q, shuffledAnswers))
                
              -- mapAccumL returns (final_accumulator, result_list)
              ((vMap', gen2), examData) = mapAccumL extractVariant (vMap, gen1) shuffledQs
              
              answerKey = map (uncurry LatexFormatter.formatCorrectAnswers) examData
              latexOutput = LatexFormatter.format (header config) latexConfig vNum examData
            in
              ((vMap', gen2), (vNum, latexOutput, answerKey))
              
      let (_, processedExams) = mapAccumL buildVersion (initialVariantMap, genShuffle) (zip [1..] examsToBuild)

      -- Write Output Files
      mapM_ (\(version, tex, _) -> writeFile (outDir </> printf "%s-%02d.tex" baseName version) tex) processedExams
      putStrLn "\nExam .tex generation complete."
      
      let allAnswerKeys = [(v, key) | (v, _, key) <- processedExams]
      writeCsvFile (outDir </> printf "%s.keys.csv" baseName) allAnswerKeys

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
    -- Find the maximum number of questions across all versions for a safe header length
    let maxQuestions = maximum $ map (length . snd) dataRows
    let csvHeader = "Exam_Type," ++ intercalate "," (map (\i -> "Q" ++ show i) [1..maxQuestions])
    let rows = map (\(v, ans) -> show v ++ "," ++ intercalate "," ans) dataRows
    writeFile path (unlines (csvHeader:rows))
    putStrLn $ "Answer key written to " ++ path
