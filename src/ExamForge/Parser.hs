{-# LANGUAGE OverloadedStrings #-}

module ExamForge.Parser where

import ExamForge.Type
import Data.Maybe (fromMaybe)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Key as Key -- Import Key to convert keys to text
import qualified Data.Aeson.KeyMap as KeyMap -- The correct KeyMap module

-- Helper function to make parsing the 'answers' list cleaner.
parseAnswer :: Object -> Parser Answer
parseAnswer obj =
  -- Use KeyMap.toList from Aeson, not Map.toList from containers.
  case KeyMap.toList obj of
    [(key, val)] ->
      case Key.toText key of
        "correct"   -> CorrectAnswer <$> parseJSON val
        "incorrect" -> IncorrectAnswer <$> parseJSON val
        _           -> fail "Answer key must be 'correct' or 'incorrect'."
    _ -> fail "Answer must be an object with a single key."

-- Instance for our custom Answer type.
instance FromJSON Answer where
  parseJSON = withObject "Answer" parseAnswer

-- Instance for SelectionType, mapping strings to our data type.
instance FromJSON SelectionType where
  parseJSON = withText "SelectionType" $ \t ->
    case t of
      "any" -> pure SelectAny
      "all" -> pure SelectAll
      _     -> fail "selection_type must be either 'any' or 'all'."

-- The main instance for our QuestionTemplate.
instance FromJSON QuestionTemplate where
  parseJSON = withObject "QuestionTemplate" $ \v -> QuestionTemplate
    <$> v .:  "id"
    <*> v .:  "title"
    <*> v .:  "format"
    <*> v .:? "subject"
    <*> (fromMaybe [] <$> v .:? "tags")
    <*> v .:  "selection_type"
    <*> (fromMaybe [] <$> v .:? "parameters")
    <*> v .:? "computations"
    <*> v .:  "question"
    <*> v .:  "answers"
    <*> (parseDelimiters =<< v .:? "delimiters")

-- | Helper to parse the delimiters object into a tuple.
parseDelimiters :: Maybe Object -> Parser (Maybe (String, String))
parseDelimiters Nothing = pure Nothing -- If the key is absent, return Nothing.
parseDelimiters (Just obj) = do
  -- Expect an object with "start" and "end" keys.
  start <- obj .: "start"
  end   <- obj .: "end"
  pure $ Just (start, end)
