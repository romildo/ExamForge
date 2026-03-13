{-# LANGUAGE OverloadedStrings #-}

module ExamForge.Template 
  ( TemplateNode(..)
  , AnswerAST(..)
  , parseTemplate 
  ) where

import qualified Data.Text as T

-- | Represents a parsed chunk of a question or answer string.
data TemplateNode
  = Literal String
  -- ^ Pure static text (e.g., "O paciente ")
  | Expression String (Maybe String)
  -- ^ An interpolated expression with its text and an optional native format hint.
  -- Example: Expression "2*width" (Just "%.2f")
  deriving (Show, Eq)

-- | Represents a parsed answer choice.
data AnswerAST = AnswerAST
  { isCorrect :: Bool
  , textNodes :: [TemplateNode]
  } deriving (Show, Eq)

-- | Parses a raw template string into an AST using custom delimiters.
parseTemplate :: (String, String) -> String -> [TemplateNode]
parseTemplate (startDelim, endDelim) template =
  let
    start = T.pack startDelim
    end   = T.pack endDelim
    
    go s | T.null s = []
         | otherwise =
           let (before, rest) = T.breakOn start s
               -- Create the literal node (only keep if not empty)
               staticNode = Literal (T.unpack before)
               staticList = if T.null before then [] else [staticNode]
           in if T.null rest
              then staticList -- No more delimiters found
              else 
                let s' = T.drop (T.length start) rest
                    (expression, after) = T.breakOn end s'
                    
                    -- Split the inner expression by ":" to find the format hint
                    (expText, fmtPart) = T.breakOn ":" expression
                    exp = T.unpack expText
                    
                    -- If there's no ":", fmtPart is empty. 
                    -- If there is a ":", T.drop 1 removes the ":" itself.
                    expNode = if T.null fmtPart
                              then Expression exp Nothing
                              else Expression exp (Just (T.unpack (T.drop 1 fmtPart)))
                              
                in staticList ++ [expNode] ++ go (T.drop (T.length end) after)
  in
    go (T.pack template)
