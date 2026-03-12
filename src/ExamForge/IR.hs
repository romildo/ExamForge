{-# LANGUAGE OverloadedStrings #-}

module ExamForge.IR 
  ( ExpandedQuestion(..)
  , VariantIR(..)
  , AnswerIR(..)
  , toCoreQuestion
  ) where

import Data.Aeson
import ExamForge.QuestionBank (SelectionType(..))
import ExamForge.Exam (Question(..), Variant)

data AnswerIR = AnswerIR
  { irCorrect :: Bool
  , irText    :: String
  } deriving (Show, Eq)

instance FromJSON AnswerIR where
  parseJSON = withObject "AnswerIR" $ \v -> AnswerIR
    <$> v .: "correct"
    <*> v .: "text"

data VariantIR = VariantIR
  { irQuestion :: String
  , irAnswers  :: [AnswerIR]
  } deriving (Show, Eq)

instance FromJSON VariantIR where
  parseJSON = withObject "VariantIR" $ \v -> VariantIR
    <$> v .: "question"
    <*> v .: "answers"

data ExpandedQuestion = ExpandedQuestion
  { irId            :: String
  , irTitle         :: String
  , irSubject       :: Maybe String
  , irTags          :: [String]
  , irSelectionType :: SelectionType
  , irVariants      :: [VariantIR]
  } deriving (Show, Eq)

instance FromJSON ExpandedQuestion where
  parseJSON = withObject "ExpandedQuestion" $ \v -> ExpandedQuestion
    <$> v .:  "id"
    <*> v .:  "title"
    <*> v .:? "subject"
    <*> v .:? "tags" .!= []
    <*> v .:  "selection_type"
    <*> v .:  "variants"

-- | Translate JSON IR directly into ExamForge's core Question type
toCoreQuestion :: ExpandedQuestion -> Question
toCoreQuestion ir = Question
  { qId            = irId ir
  , qTitle         = irTitle ir
  , qSubject       = irSubject ir
  , qTags          = irTags ir
  , qSelectionType = irSelectionType ir
  , qVariants      = map toCoreVariant (irVariants ir)
  }

-- | Translate a VariantIR into the tuple format expected by Latex.hs
toCoreVariant :: VariantIR -> Variant
toCoreVariant vIr = 
  let coreAnswers = map (\a -> (irCorrect a, irText a)) (irAnswers vIr)
  in (irQuestion vIr, coreAnswers)
