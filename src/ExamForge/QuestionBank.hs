{-# LANGUAGE OverloadedStrings #-}

module ExamForge.QuestionBank 
  ( QuestionTemplate(..)
  , SelectionType(..)
  , Delimiters(..)
  , ParameterBlock(..)
  , Answer(..)
  , loadQuestionBank
  ) where

import Data.Aeson
import Data.Yaml (decodeFileThrow)
import Data.Map (Map)
import qualified Data.Map as Map

-- | Defines how correctness is evaluated.
data SelectionType = SelectAny | SelectAll
  deriving (Show, Eq, Ord)

instance FromJSON SelectionType where
  parseJSON = withText "SelectionType" $ \t ->
    case t of
      "any" -> return SelectAny
      "all" -> return SelectAll
      _     -> fail $ "Invalid selection_type (must be either 'any' or 'all'): " ++ show t

-- | The delimiters object
data Delimiters = Delimiters
  { startDelim :: String
  , endDelim   :: String
  } deriving (Show, Eq, Ord)

instance FromJSON Delimiters where
  parseJSON = withObject "Delimiters" $ \v -> Delimiters
    <$> v .: "start"
    <*> v .: "end"

-- | The Parameter Block
data ParameterBlock = ParameterBlock
  { paramTypes :: Map String String
  , paramRows  :: [Map String String]
  } deriving (Show, Eq, Ord)

instance FromJSON ParameterBlock where
  parseJSON = withObject "ParameterBlock" $ \v -> ParameterBlock
    <$> v .:? "types" .!= Map.empty
    <*> v .:  "rows"

-- | The Answer: a single answer choice, flagging it as either correct or incorrect
data Answer
  = CorrectAnswer String
  | IncorrectAnswer String
  deriving (Show, Eq)

instance FromJSON Answer where
  parseJSON = withObject "Answer" $ \v -> do
    let parseCorrect = CorrectAnswer <$> v .: "correct"
    let parseIncorrect = IncorrectAnswer <$> v .: "incorrect"
    parseCorrect <> parseIncorrect

-- | The main data structure that represents a question template
data QuestionTemplate = QuestionTemplate
  { qtId          :: String
  , qtTitle       :: String
  , qtFormat      :: String
  , qtLanguage    :: Maybe String
  , qtSelection   :: SelectionType
  , qtSubject     :: Maybe String
  , qtTags        :: [String]
  , qtParameters  :: Maybe ParameterBlock
  , qtComputations:: Maybe String
  , qtQuestion    :: String
  , qtAnswers     :: [Answer]
  , qtDelimiters  :: Maybe Delimiters
  } deriving (Show, Eq)

instance FromJSON QuestionTemplate where
  parseJSON = withObject "QuestionTemplate" $ \v -> QuestionTemplate
    <$> v .:  "id"
    <*> v .:  "title"
    <*> v .:? "format" .!= "latex"
    <*> v .:? "language"
    <*> v .:  "selection_type"
    <*> v .:? "subject"
    <*> v .:? "tags" .!= []
    <*> v .:? "parameters"
    <*> v .:? "computations"
    <*> v .:  "question"
    <*> v .:  "answers"
    <*> v .:? "delimiters"

-- | Replaces your old ExamForge.Parser logic
loadQuestionBank :: FilePath -> IO [QuestionTemplate]
loadQuestionBank filepath = do
  -- decodeFileThrow automatically handles YAML parsing and throws helpful 
  -- exceptions if the file is missing or the schema is wrong.
  decodeFileThrow filepath
