-- File: src/ExamForge/ExamConfig.hs

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExamForge.ExamConfig
  ( Config(..)
  , Header(..)
  , AssemblyOptions(..)
  , SemanticGroupRule(..)
  , Selection(..)
  , Content(..)
  , loadConfig
  ) where

import Data.Yaml
import GHC.Generics
import Control.Monad (when)

-- | Corresponds to the 'header' section of the YAML file.
data Header = Header
  { institution :: String
  , course :: String
  , professor :: String
  , semester :: String
  , title :: String
  } deriving (Show, Generic)

instance FromJSON Header

-- | Corresponds to the 'assembly_options' section.
data AssemblyOptions = AssemblyOptions
  { versions            :: Int
  , show_id             :: Bool
  , show_tags           :: Bool
  , hide_subjects       :: Bool
  , shuffle_questions   :: Bool
  , seed                :: Maybe Int -- Optional random seed
  , registration_digits :: Maybe Int -- Optional field for OMR grid
  } deriving (Show, Generic)

-- | Default values for AssemblyOptions.
defaultAssemblyOptions :: AssemblyOptions
defaultAssemblyOptions = AssemblyOptions
  { versions            = 1
  , show_id             = False
  , show_tags           = False
  , hide_subjects       = False
  , shuffle_questions   = True
  , seed                = Nothing
  , registration_digits = Nothing -- Default to no grid
  }

instance FromJSON AssemblyOptions where
  parseJSON = withObject "AssemblyOptions" $ \v -> AssemblyOptions
    <$> v .:? "versions"            .!= versions defaultAssemblyOptions
    <*> v .:? "show_id"             .!= show_id defaultAssemblyOptions
    <*> v .:? "show_tags"           .!= show_tags defaultAssemblyOptions
    <*> v .:? "hide_subjects"       .!= hide_subjects defaultAssemblyOptions
    <*> v .:? "shuffle_questions"   .!= shuffle_questions defaultAssemblyOptions
    <*> v .:? "seed"                .!= seed defaultAssemblyOptions
    <*> v .:? "registration_digits" .!= registration_digits defaultAssemblyOptions

-- | Corresponds to the 'semantic_group' section within 'selection'.
data SemanticGroupRule = SemanticGroupRule
  { tag_pattern :: String
  , maximum_qs :: Int
  } deriving (Show, Generic)

instance FromJSON SemanticGroupRule where
  parseJSON = withObject "SemanticGroupRule" $ \v -> do
    pattern <- v .: "pattern"
    maxQs <- v .:? "maximum" .!= 1
    when (maxQs < 1) $ fail "maximum must be >= 1"
    pure $ SemanticGroupRule pattern maxQs

-- | Corresponds to the 'selection' section.
data Selection = Selection
  { include_tags :: [String]
  , exclude_tags :: [String]
  , semantic_groups :: [SemanticGroupRule]
  } deriving (Show, Generic)

defaultSelection :: Selection
defaultSelection = Selection [] [] []

instance FromJSON Selection where
  parseJSON = withObject "Selection" $ \v -> Selection
    <$> v .:? "include_tags"   .!= include_tags defaultSelection
    <*> v .:? "exclude_tags"   .!= exclude_tags defaultSelection
    <*> v .:? "semantic_groups" .!= semantic_groups defaultSelection

-- | Corresponds to the 'content' section.
data Content = Content
  { instructions   :: String
  , latex_preamble :: String  -- Código a ser injetado no preâmbulo
  } deriving (Show, Generic)

-- | Default values for Content.
defaultContent :: Content
defaultContent = Content
  { instructions   = ""
  , latex_preamble = ""       -- Default é vazio
  }

instance FromJSON Content where
  parseJSON = withObject "Content" $ \v -> Content
    <$> v .:? "instructions"   .!= instructions defaultContent
    <*> v .:? "latex_preamble" .!= latex_preamble defaultContent

-- | Main configuration structure. Now handles optional sections.
data Config = Config
  { header :: Header
  , question_banks :: [FilePath]
  , assembly_options :: AssemblyOptions
  , selection :: Selection
  , content :: Content
  } deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config
    <$> v .:  "header"
    <*> v .:  "question_banks"
    <*> v .:? "assembly_options" .!= defaultAssemblyOptions
    <*> v .:? "selection"        .!= defaultSelection
    <*> v .:? "content"          .!= defaultContent

-- | Loads and parses an exam configuration file.
loadConfig :: FilePath -> IO (Either ParseException Config)
loadConfig = decodeFileEither
