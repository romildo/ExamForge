module ExamForge.Evaluator.Class 
  ( Evaluator(..)
  ) where

import ExamForge.QuestionBank (QuestionTemplate)
import ExamForge.Template (TemplateNode, AnswerAST)

class Evaluator e where
  -- | The unique string identifier for the language (e.g., "python", "c")
  evaluatorId :: e -> String

  -- | Generates the complete source code script for the target language.
  generateScript :: e
                 -> QuestionTemplate      -- ^ The full question metadata and parameters
                 -> [TemplateNode]        -- ^ The parsed AST of the question text
                 -> [AnswerAST]           -- ^ The parsed ASTs of the answers
                 -> String                -- ^ The generated source code
