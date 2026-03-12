module ExamForge.Evaluator.C (CEval(..)) where

import ExamForge.Evaluator.Class
import ExamForge.QuestionBank
import ExamForge.Template
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

data CEval = CEval

instance Evaluator CEval where
  evaluatorId _ = "c"

  generateScript _ qt qNodes aNodes =
    let
      (paramNames, rows, pTypes) = case qtParameters qt of
        Nothing -> ([], [], M.empty)
        Just pb -> (M.keys (head (paramRows pb)), paramRows pb, paramTypes pb)

      -- Generate function signature: e.g., void mkVariant(int offset, int base, int is_last)
      args = map (\name -> M.findWithDefault "int" name pTypes ++ " " ++ name) paramNames
      signatureArgs = intercalate ", " (args ++ ["int is_last"])

      computationsCode = case qtComputations qt of
        Nothing -> ""
        Just c  -> unlines . map ("    " ++) . lines $ c

      -- Map AST to printf statements
      printAST nodes = unlines $ map nodeToC nodes
      nodeToC (Literal text) = "    printf(\"%s\", \"" ++ escapeJSONForC text ++ "\");"
      nodeToC (Variable name Nothing) = "    printf(\"%d\", " ++ name ++ ");" -- fallback
      nodeToC (Variable name (Just fmt)) = "    printf(\"" ++ fmt ++ "\", " ++ name ++ ");"

      buildAnswers = unlines $ zipWith buildAns aNodes [1..length aNodes]
      buildAns (AnswerAST isCorr nodes) idx =
        let isLastAns = idx == length aNodes
            boolStr = if isCorr then "true" else "false"
        in unlines
           [ "    printf(\"    {\\\"correct\\\": " ++ boolStr ++ ", \\\"text\\\": \\\"\");"
           , printAST nodes
           , "    printf(\"\\\"}%s\\n\", " ++ (if isLastAns then "\"\"" else "\",\"") ++ ");"
           ]

      selectionString = case qtSelection qt of
        SelectAny -> "any"
        SelectAll -> "all"

    in unlines
      [ "#include <stdio.h>"
      , "#include <stdlib.h>"
      , "#include <stdbool.h>"
      , ""
      , "void mkVariant(" ++ signatureArgs ++ ") {"
      , computationsCode
      , "    printf(\"  {\\n\");"
      , "    printf(\"    \\\"question\\\": \\\"\");"
      , printAST qNodes
      , "    printf(\"\\\",\\n\");"
      , "    printf(\"    \\\"answers\\\": [\\n\");"
      , buildAnswers
      , "    printf(\"    ]\\n\");"
      , "    printf(\"  }%s\\n\", is_last ? \"\" : \",\");"
      , "}"
      , ""
      , "int main() {"
      , "  printf(\"{\\n\");"
      , "  printf(\"  \\\"id\\\": \\\"" ++ escapeJSONForC (qtId qt) ++ "\\\",\\n\");"
      , "  printf(\"  \\\"title\\\": \\\"" ++ escapeJSONForC (qtTitle qt) ++ "\\\",\\n\");"
      , "  printf(\"  \\\"subject\\\": \\\"" ++ escapeJSONForC (fromMaybe "" (qtSubject qt)) ++ "\\\",\\n\");"
      , "  printf(\"  \\\"tags\\\": [" ++ intercalate ", " (map (\t -> "\\\"" ++ escapeJSONForC t ++ "\\\"") (qtTags qt)) ++ "],\\n\");"
      , "  printf(\"  \\\"selection_type\\\": \\\"" ++ selectionString ++ "\\\",\\n\");"
      , "  printf(\"  \\\"variants\\\": [\\n\");"
      , generateExecutionLoop paramNames rows
      , "  printf(\"  ]\\n\");"
      , "  printf(\"}\\n\");"
      , "  return 0;"
      , "}"
      ]

-- | Double-escapes strings so they survive BOTH the C compiler and the JSON parser
escapeJSONForC :: String -> String
escapeJSONForC = concatMap escapeChar
  where
    escapeChar '\"' = "\\\\\\\"" -- Outputs \\\" to the .c file -> C memory stores \" -> JSON parser sees valid quote
    escapeChar '\\' = "\\\\\\\\" -- Outputs \\\\ to the .c file -> C memory stores \\ -> JSON parser sees valid backslash
    escapeChar '\n' = "\\\\n"    -- Outputs \\n  to the .c file -> C memory stores \n -> JSON parser sees \n
    escapeChar '\r' = ""
    escapeChar '\t' = "\\\\t"
    escapeChar c    = [c]

generateExecutionLoop :: [String] -> [M.Map String String] -> String
generateExecutionLoop _ [] = "  mkVariant(1);"
generateExecutionLoop paramNames rows =
  let buildCall row idx =
        let isLast = if idx == length rows then "1" else "0"
            vals = map (\k -> M.findWithDefault "0" k row) paramNames
        in "  mkVariant(" ++ intercalate ", " vals ++ ", " ++ isLast ++ ");"
  in unlines $ zipWith buildCall rows [1..length rows]
