module ExamForge.Formatter.Latex where

import ExamForge.Exam (Question(..), Variant)
import ExamForge.Type (SelectionType(..))

import Data.List (findIndex, intercalate, sort)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)

-- Configuration for the formatter to control metadata visibility.
data FormatterConfig = FormatterConfig
  { showId      :: Bool
  , showTags    :: Bool
  , showSubject :: Bool
  } deriving (Show)

-- | The main function that generates the complete .tex file string.
format :: FormatterConfig -> Int -> [(Question, Variant)] -> String
format config version examData =
  let
    -- Generate the answer key strings (e.g., "A+C", "BE") from the exam data.
    answerKey = map (uncurry formatCorrectAnswers) examData
  in
    unlines
      [ "\\documentclass[a4paper,10pt,brazil]{article}"
      , "\\usepackage{provastyle}\n"
      , "\\begin{document}\n"
      , printf "\\cabeçalho{B-%02d}\n" version
      , "%\\begin{multicols}{2}\n"
      , formatQuestions config examData 1
      , "%\\end{multicols}\n"
      , printf "\\folhadeRespostasDinamica{B-%02d}{%d}{25}\n" version (length examData)
      , formatAnswerKey version answerKey
      , "\\end{document}"
      ]

-- | Takes a Question and a Variant and formats the correct answer(s) as a string.
formatCorrectAnswers :: Question -> Variant -> String
formatCorrectAnswers question (_, variantAnswers) =
  let
    -- Find the 0-based indices of all correct answers.
    correctIndices = [i | (i, (isCorrect, _)) <- zip [0..] variantAnswers, isCorrect]
    -- Convert indices to characters: [0, 2] -> ['A', 'C']
    answerChars = map (\idx -> toEnum (fromEnum 'A' + idx)) correctIndices
    -- Sort the characters to ensure consistent output (e.g., "A+C", not "C+A").
    sortedChars = sort answerChars
  in
    -- Join the characters based on the question's selection type.
    case qSelectionType question of
      SelectAny -> intercalate "+" [[c] | c <- sortedChars] -- e.g., "A+C"
      SelectAll -> concat [[c] | c <- sortedChars]       -- e.g., "AC"

-- | Formats the list of questions for the exam body.
formatQuestions :: FormatterConfig -> [(Question, Variant)] -> Int -> String
formatQuestions _ [] _ = ""
formatQuestions config ((question, (variantText, variantAnswers)):qs) n =
  let
    -- Construct the title string with optional metadata.
    metaParts =
      [ if showSubject config then qSubject question else Nothing
      , if showId config then Just (qId question) else Nothing
      , if showTags config then Just (unwords (qTags question)) else Nothing
      ]
    titleString = intercalate " | " [part | Just part <- metaParts, not (null part)]

    -- Get the text of each answer choice.
    answerTexts = map snd variantAnswers
  in
    unlines
    [ printf "\\questionTitle{%d}{%s}" n titleString
    , variantText
    , "\\begin{enumerate}"
    , unlines (map (\alt -> "  \\item " ++ alt) answerTexts)
    , "\\end{enumerate}\n"
    ] ++ formatQuestions config qs (n+1)

-- | Formats the answer key for the '\gabarito' macro.
formatAnswerKey :: Int -> [String] -> String
formatAnswerKey version answerStrings =
  let answersCsv = intercalate "," answerStrings
  in printf "\\gabarito{B-%02d}{%s}" version answersCsv
