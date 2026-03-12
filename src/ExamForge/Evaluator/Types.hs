module ExamForge.Evaluator.Types where

import qualified Data.Map as M
import ExamForge.Template.AST (TemplateNode, AnswerAST)

-- | Metadata that the target script must echo back into the JSON IR.
data QuestionMetadata = QuestionMetadata
  { qId            :: String
  , qTitle         :: String
  , qSubject       :: Maybe String
  , qTags          :: [String]
  , qSelectionType :: String
  } deriving (Show, Eq)

-- | The parameters required to generate the variants.
data ParameterBlock = ParameterBlock
  { paramTypes :: M.Map String String       -- ^ e.g., {"offset": "int", "base": "int"}
  , paramRows  :: [M.Map String String]     -- ^ e.g., [{"offset": "3", "base": "1000"}, ...]
  } deriving (Show, Eq)