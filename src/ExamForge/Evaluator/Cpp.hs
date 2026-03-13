module ExamForge.Evaluator.Cpp (CppEval(..)) where

import ExamForge.Evaluator.Class
import ExamForge.QuestionBank
import ExamForge.Template
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

data CppEval = CppEval

instance Evaluator CppEval where
  evaluatorId _ = "cpp"

  generateScript _ qt qNodes aNodes =
    let
      (paramNames, rows, pTypes) = case qtParameters qt of
        Nothing -> ([], [], M.empty)
        Just pb -> (M.keys (head (paramRows pb)), paramRows pb, paramTypes pb)

      -- Generate function signature: e.g., void mkVariant(int offset, std::string name, bool is_last)
      args = map (\name -> M.findWithDefault "int" name pTypes ++ " " ++ name) paramNames
      signatureArgs = intercalate ", " (args ++ ["bool is_last"])

      computationsCode = case qtComputations qt of
        Nothing -> ""
        Just c  -> unlines . map ("    " ++) . lines $ c

      printAST nodes = unlines $ map nodeToCpp nodes
      nodeToCpp (Literal text) = "    std::cout << \"" ++ escapeJSONForCpp text ++ "\";"
      
      -- Native C++ streams handle std::string, floats, and ints automatically!
      nodeToCpp (Expression code Nothing) = "    std::cout << (" ++ code ++ ");"
      
      -- If the user provides a format hint, we fall back to printf
      nodeToCpp (Expression code (Just fmt)) = "    printf(\"" ++ fmt ++ "\", " ++ code ++ ");"

      buildAnswers = unlines $ zipWith buildAns aNodes [1..length aNodes]
      buildAns (AnswerAST isCorr nodes) idx =
        let isLastAns = idx == length aNodes
            boolStr = if isCorr then "true" else "false"
        in unlines
           [ "    std::cout << \"    {\\\"correct\\\": " ++ boolStr ++ ", \\\"text\\\": \\\"\";"
           , printAST nodes
           , "    std::cout << \"\\\"}\" << (" ++ (if isLastAns then "true" else "false") ++ " ? \"\" : \",\") << \"\\n\";"
           ]

      selectionString = case qtSelection qt of
        SelectAny -> "any"
        SelectAll -> "all"

    in unlines
      [ "#include <iostream>"
      , "#include <string>"
      , "#include <vector>"
      , "#include <cstdio>"
      , "#include <cmath>"
      , "#include <algorithm>"
      , ""
      , "using namespace std;"
      , ""
      , "void mkVariant(" ++ signatureArgs ++ ") {"
      , computationsCode
      , "    std::cout << \"  {\\n\";"
      , "    std::cout << \"    \\\"question\\\": \\\"\";"
      , printAST qNodes
      , "    std::cout << \"\\\",\\n\";"
      , "    std::cout << \"    \\\"answers\\\": [\\n\";"
      , buildAnswers
      , "    std::cout << \"    ]\\n\";"
      , "    std::cout << \"  }\" << (is_last ? \"\" : \",\") << \"\\n\";"
      , "}"
      , ""
      , "int main() {"
      , "  std::cout << \"{\\n\";"
      , "  std::cout << \"  \\\"id\\\": \\\"" ++ escapeJSONForCpp (qtId qt) ++ "\\\",\\n\";"
      , "  std::cout << \"  \\\"title\\\": \\\"" ++ escapeJSONForCpp (qtTitle qt) ++ "\\\",\\n\";"
      , "  std::cout << \"  \\\"subject\\\": \\\"" ++ escapeJSONForCpp (fromMaybe "" (qtSubject qt)) ++ "\\\",\\n\";"
      , "  std::cout << \"  \\\"tags\\\": [" ++ intercalate ", " (map (\t -> "\\\"" ++ escapeJSONForCpp t ++ "\\\"") (qtTags qt)) ++ "],\\n\";"
      , "  std::cout << \"  \\\"selection_type\\\": \\\"" ++ selectionString ++ "\\\",\\n\";"
      , "  std::cout << \"  \\\"variants\\\": [\\n\";"
      , generateExecutionLoop paramNames rows
      , "  std::cout << \"  ]\\n\";"
      , "  std::cout << \"}\\n\";"
      , "  return 0;"
      , "}"
      ]

-- Same escaping logic as C
escapeJSONForCpp :: String -> String
escapeJSONForCpp = concatMap escapeChar
  where
    escapeChar '\"' = "\\\\\\\""
    escapeChar '\\' = "\\\\\\\\"
    escapeChar '\n' = "\\\\n"
    escapeChar '\r' = ""
    escapeChar '\t' = "\\\\t"
    escapeChar c    = [c]

generateExecutionLoop :: [String] -> [M.Map String String] -> String
generateExecutionLoop _ [] = "  mkVariant(true);"
generateExecutionLoop paramNames rows =
  let buildCall row idx =
        let isLast = if idx == length rows then "true" else "false"
            -- Provide a fallback to "0" if the YAML cell is somehow empty
            vals = map (\k -> M.findWithDefault "0" k row) paramNames
        in "  mkVariant(" ++ intercalate ", " vals ++ ", " ++ isLast ++ ");"
  in unlines $ zipWith buildCall rows [1..length rows]
