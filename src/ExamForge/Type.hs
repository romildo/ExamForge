module ExamForge.Type where

-- We will use Data.Map to represent parameters, which are key-value maps.
-- Aeson (which we'll use for YAML parsing) works well with the Value type
-- for heterogeneous parameter values (strings, numbers, etc.).
import Data.Aeson (Value)
import Data.Map (Map)

-- The selection criteria for answers in a multiple-choice question.
data SelectionType
  = SelectAny -- Corresponds to "any" in YAML
  | SelectAll -- Corresponds to "all" in YAML
  deriving (Show, Eq)

-- Represents a single answer choice, flagging it as either correct or incorrect.
data Answer
  = CorrectAnswer { content :: String }
  | IncorrectAnswer { content :: String }
  deriving (Show, Eq)

-- The main data structure that represents a question template,
-- directly corresponding to our YAML specification.
data QuestionTemplate = QuestionTemplate
  { qtId           :: String
  , qtTitle        :: String
  , qtFormat       :: String
  , qtSubject      :: Maybe String
  , qtTags         :: [String]
  , qtSelection    :: SelectionType
  , qtParameters   :: [Map String Value]
  , qtComputations :: Maybe String
  , qtQuestion     :: String
  , qtAnswers      :: [Answer]
  , qtDelimiters   :: Maybe (String, String)
  } deriving (Show, Eq)
