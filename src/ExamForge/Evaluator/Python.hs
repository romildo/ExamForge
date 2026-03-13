module ExamForge.Evaluator.Python 
  ( PythonEval(..) 
  ) where

import ExamForge.Evaluator.Class
import ExamForge.QuestionBank
import ExamForge.Template
import Data.List (intercalate)
import qualified Data.Map as M

data PythonEval = PythonEval

instance Evaluator PythonEval where
  evaluatorId _ = "python"

  generateScript _ qt qNodes aNodes =
    let
      (paramNames, rows) = case qtParameters qt of
        Nothing -> ([], [])
        Just pb -> 
          let r = paramRows pb
          in if null r 
             then ([], []) 
             else (M.keys (head r), r)

      signatureArgs = intercalate ", " paramNames
      
      computationsCode = case qtComputations qt of
        Nothing -> "    pass"
        Just c  -> unlines . map ("    " ++) . lines $ c

      qString = "    q_text = " ++ buildPythonString qNodes
      aStrings = map buildAnswerString aNodes

      selectionString = case qtSelection qt of
        SelectAny -> "any"
        SelectAll -> "all"

    in unlines
      [ "import json"
      , ""
      , "# --- 1. VARIANT GENERATOR FUNCTION ---"
      , "def mkVariant(" ++ signatureArgs ++ "):"
      , computationsCode
      , ""
      , "# --- 2. HASKELL-GENERATED STRING INTERPOLATION ---"
      , qString
      , "    ans = []"
      , unlines aStrings
      , "    return {"
      , "        \"question\": q_text,"
      , "        \"answers\": ans"
      , "    }"
      , ""
      , "# --- 3. PARAMETER EXECUTION ---"
      , "variants = []"
      , generateExecutionLoop paramNames rows
      , ""
      , "# --- 4. OUTPUT GENERATION ---"
      , "output = {"
      , "    \"id\": " ++ safePyString (qtId qt) ++ ","
      , "    \"title\": " ++ safePyString (qtTitle qt) ++ ","
      , "    \"subject\": " ++ safePyMaybe (qtSubject qt) ++ ","
      , "    \"tags\": " ++ safePyList (qtTags qt) ++ ","
      , "    \"selection_type\": " ++ safePyString selectionString ++ ","
      , "    \"variants\": variants"
      , "}"
      , ""
      , "print(json.dumps(output))"
      ]

-- | Safely encodes a Haskell string into a raw Python triple-quoted string
safePyString :: String -> String
safePyString text = "\"\"\"" ++ escape text ++ "\"\"\""
  where
    -- Escape both quotes and backslashes to protect LaTeX commands
    escape = concatMap (\c -> case c of
                                '\"' -> "\\\""
                                '\\' -> "\\\\"
                                _    -> [c])

safePyMaybe :: Maybe String -> String
safePyMaybe Nothing  = "None"
safePyMaybe (Just t) = safePyString t

safePyList :: [String] -> String
safePyList xs = "[" ++ intercalate ", " (map safePyString xs) ++ "]"

-- | Helper: Translates a list of AST nodes into safe Python concatenation
buildPythonString :: [TemplateNode] -> String
buildPythonString [] = "\"\""
buildPythonString nodes = intercalate " + " (map nodeToPython nodes)
  where
    nodeToPython (Literal text) = safePyString text
    nodeToPython (Expression code Nothing)    = "f\"{" ++ code ++ "}\""
    nodeToPython (Expression code (Just fmt)) = "f\"{" ++ code ++ ":" ++ fmt ++ "}\""

buildAnswerString :: AnswerAST -> String
buildAnswerString (AnswerAST isCorr nodes) =
  let boolStr = if isCorr then "True" else "False"
      textStr = buildPythonString nodes
  in "    ans.append({\"correct\": " ++ boolStr ++ ", \"text\": str(" ++ textStr ++ ")})"

generateExecutionLoop :: [String] -> [M.Map String String] -> String
generateExecutionLoop _ [] = "variants.append(mkVariant())"
generateExecutionLoop paramNames rows =
  unlines $ map buildCall rows
  where
    buildCall row = 
      let vals = map (\k -> M.findWithDefault "None" k row) paramNames
      in "variants.append(mkVariant(" ++ intercalate ", " vals ++ "))"
