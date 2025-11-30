module ExamForge.Exam where

import ExamForge.Type (SelectionType)

-- A Variant represents one concrete, fully generated instance of a question.
type Variant =
  ( String   -- The final question text, with markup (e.g., LaTeX).
  , [(Bool, String)] -- List of choices: (is_correct, text).
  )

-- A Question represents the collection of all possible variants for a single
-- template, carrying over all necessary metadata for the orchestrator.
data Question = Question
  { qId            :: String
  , qTitle         :: String
  , qSubject       :: Maybe String
  , qTags          :: [String]
  , qSelectionType :: SelectionType
  , qVariants      :: [Variant]
  } deriving (Show, Eq)
