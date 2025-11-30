{-# LANGUAGE OverloadedStrings #-}

module ExamForge.Generator where

import ExamForge.Type

import Data.Aeson (Value(..))
import Data.Char (toUpper)
import Data.List (intercalate, sortOn)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.Text as T
import Text.Printf (printf)

-- A helper to check if all elements in a list are equal.
allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (== x) xs

-- A helper to indent a multi-line string.
indent :: Int -> String -> String
indent n s = unlines (map (replicate n ' ' ++) (lines s))

betweenWithSep :: [a] -> [a] -> [a] -> [[a]] -> [a]
betweenWithSep left right sep xs = left ++ intercalate sep xs ++ right

formatList :: [String] -> String
formatList = betweenWithSep "[" "]" "," 

-- buildFromTemplate now takes the delimiters as arguments.
buildFromTemplate :: (String, String) -> String -> String
buildFromTemplate (startDelim, endDelim) template =
  let
    start = T.pack startDelim
    end = T.pack endDelim
    go s | T.null s = []
         | otherwise =
           let (before, rest) = T.breakOn start s
               staticPart = T.pack . show . T.unpack $ before
           in if T.null rest
              then [staticPart]
              else let s' = T.drop (T.length start) rest
                       (expression, after) = T.breakOn end s'
                       dynamicPart = T.concat ["(", expression, ")"]
                   in if T.null before
                      then dynamicPart : go (T.drop (T.length end) after)
                      else staticPart : dynamicPart : go (T.drop (T.length end) after)
  in
    T.unpack . T.intercalate " ++ " . go . T.pack $ template

aesonValueToHaskellString :: Value -> String
aesonValueToHaskellString (String t) = show t
aesonValueToHaskellString (Number n) = show n
aesonValueToHaskellString (Bool b)   = show b
aesonValueToHaskellString Null       = "error \"Null parameter not supported\""
aesonValueToHaskellString _          = "error \"Unsupported parameter value type\""

-- Processes the parameter list: sorts, validates, and separates names from values.
processParameters :: [Map String Value] -> ([String], String)
processParameters [] = ([], "[[]]") -- Static questions get one empty variant list
processParameters allParams
  | allEqual (map (map fst) sortedParamLists) = (paramNames, paramListString)
  | otherwise = error "Inconsistent parameter names across variants."
  where
    sortedParamLists = map (sortOn fst . Map.toList) allParams
    paramNames = map fst (head sortedParamLists)
    listOfValueLists = map (map (aesonValueToHaskellString . snd)) sortedParamLists
    -- Manually build the Haskell list syntax
    paramListString = formatList (map formatList listOfValueLists)

buildComputations :: Maybe String -> String
buildComputations Nothing = ""
buildComputations (Just computations) =
  let indented = unlines . map ("            " ++) . lines $ computations
  in indented

-- buildAnswers now needs the delimiters to pass them to buildFromTemplate.
buildAnswers :: (String, String) -> [Answer] -> String
buildAnswers delims answers =
  let
    buildAnswer (CorrectAnswer t)   = printf "(True, %s)" (buildFromTemplate delims t)
    buildAnswer (IncorrectAnswer t) = printf "(False, %s)" (buildFromTemplate delims t)
  in "[" ++ intercalate ",\n" (map buildAnswer answers) ++ "]"

moduleName :: QuestionTemplate -> String
moduleName qt =
  let
    capitalize (h:t) = toUpper h : t; capitalize "" = ""
    parts = T.splitOn "-" (T.pack $ qtId qt)
  in
    intercalate "_" $ map (capitalize . T.unpack) parts

-- Takes a list of question templates and generates a single Haskell module.
generateQuestionsModule :: String -> [QuestionTemplate] -> String
generateQuestionsModule moduleName templates =
  unlines
    [ "module " ++ moduleName ++ " where"
    , ""
    , "import ExamForge.Exam (Question(..), Variant)"
    , "import ExamForge.Type (SelectionType(..))"
    , ""
    , "-- | The complete pool of generated questions."
    , "questionPool :: [Question]"
    , "questionPool = "
    , "  [ " ++ intercalate "\n  , " (map generateQuestionRecord templates)
    , "  ]"
    ]

-- Generates the Haskell record expression for a single Question.
generateQuestionRecord :: QuestionTemplate -> String
generateQuestionRecord qt =
  let
    -- Define default delimiters and use them if none are provided.
    delims = fromMaybe ("{{", "}}") (qtDelimiters qt)
    (paramNames, paramValues) = processParameters (qtParameters qt)
  in unlines
    [ "Question"
    , "  { qId            = " ++ show (qtId qt)
    , "  , qTitle         = " ++ show (qtTitle qt)
    , "  , qSubject       = " ++ show (qtSubject qt)
    , "  , qTags          = " ++ show (qtTags qt)
    , "  , qSelectionType = " ++ show (qtSelection qt)
    , "  , qVariants      = "
    , "      let"
    , "        generateVariant [" ++ intercalate "," paramNames ++ "] ="
    , "          let"
    , buildComputations (qtComputations qt)
    , "          in"
    , "            ( " ++ buildFromTemplate delims (qtQuestion qt) -- Pass delims
    , "            ,"
    , indent 14 (buildAnswers delims (qtAnswers qt)) -- Pass delims
    , "            )"
    , "      in"
    , "        [ generateVariant paramValues | paramValues <-"
    , "            " ++ paramValues
    , "        ]"
    , "  }"
    ]
