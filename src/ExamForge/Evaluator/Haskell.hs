module ExamForge.Evaluator.Haskell (HaskellEval(..)) where

import ExamForge.Evaluator.Class
import ExamForge.QuestionBank
import ExamForge.Template
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

data HaskellEval = HaskellEval

instance Evaluator HaskellEval where
  evaluatorId _ = "haskell"

  generateScript _ qt qNodes aNodes =
    let
      (paramNames, rows) = case qtParameters qt of
        Nothing -> ([], [])
        Just pb -> (M.keys (head (paramRows pb)), paramRows pb)

      computationsCode = case qtComputations qt of
        Nothing -> ""
        Just c  -> unlines . map ("        " ++) . lines $ c

      printAST nodes = unlines $ map nodeToHs nodes
      nodeToHs (Literal text) = "      putStr \"" ++ escapeHsJSONString text ++ "\""
      nodeToHs (Variable name Nothing) = "      putStr (show " ++ name ++ ")"
      nodeToHs (Variable name (Just fmt)) = "      printf \"" ++ fmt ++ "\" " ++ name

      buildAnswers = unlines $ zipWith buildAns aNodes [1..length aNodes]
      buildAns (AnswerAST isCorr nodes) idx =
        let isLastAns = idx == length aNodes
            boolStr = if isCorr then "true" else "false"
        in unlines
           [ "      putStr \"    {\\\"correct\\\": " ++ boolStr ++ ", \\\"text\\\": \\\"\""
           , printAST nodes
           , "      putStrLn \"\\\"}" ++ (if isLastAns then "" else ",") ++ "\""
           ]

      selectionString = case qtSelection qt of
        SelectAny -> "any"
        SelectAll -> "all"

      -- Generates an inline block for a specific row to leverage GHC type inference
      generateRow row idx =
        let isLast = null rows || idx == length rows
            -- Indent bindings by 8 spaces to align under the 'let'
            bindings = map (\k -> "        " ++ k ++ " = " ++ M.findWithDefault "undefined" k row) paramNames
            letBlock = if null bindings 
                       then "" 
                       else "      let\n" ++ intercalate "\n" bindings
        in unlines
           [ "  do"               -- Open a nested do block for this row
           , letBlock               -- Insert the let bindings (no 'in' keyword needed!)
           , computationsCode       -- Insert the user's computations
           , "      putStrLn \"  {\""
           , "      putStr \"    \\\"question\\\": \\\"\""
           , printAST qNodes
           , "      putStrLn \"\\\",\""
           , "      putStrLn \"    \\\"answers\\\": [\""
           , buildAnswers
           , "      putStrLn \"    ]\""
           , "      putStrLn \"  }" ++ (if isLast then "" else ",") ++ "\""
           ]

    in unlines
      [ "import Text.Printf (printf)"
      , ""
      , "main :: IO ()"
      , "main = do"
      , "  putStrLn \"{\""
      , "  putStrLn \"  \\\"id\\\": \\\"" ++ escapeHsJSONString (qtId qt) ++ "\\\",\""
      , "  putStrLn \"  \\\"title\\\": \\\"" ++ escapeHsJSONString (qtTitle qt) ++ "\\\",\""
      , "  putStrLn \"  \\\"subject\\\": \\\"" ++ escapeHsJSONString (fromMaybe "" (qtSubject qt)) ++ "\\\",\""
      , "  putStrLn \"  \\\"tags\\\": [" ++ intercalate ", " (map (\t -> "\\\"" ++ escapeHsJSONString t ++ "\\\"") (qtTags qt)) ++ "],\""
      , "  putStrLn \"  \\\"selection_type\\\": \\\"" ++ selectionString ++ "\\\",\""
      , "  putStrLn \"  \\\"variants\\\": [\""
      , if null rows then generateRow M.empty 1 else unlines (zipWith generateRow rows [1..length rows])
      , "  putStrLn \"  ]\""
      , "  putStrLn \"}\""
      ]

escapeHsJSONString :: String -> String
escapeHsJSONString = concatMap escapeChar
  where
    escapeChar '\"' = "\\\\\\\""
    escapeChar '\\' = "\\\\\\\\"
    escapeChar '\n' = "\\\\n"
    escapeChar '\r' = ""
    escapeChar '\t' = "\\\\t"
    escapeChar c    = [c]
