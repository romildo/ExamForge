{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExamConfig
  ( Config(..)
  , Header(..)
  , AssemblyOptions(..)
  , Selection(..)
  , Content(..)
  , loadConfig
  ) where

import Data.Yaml
import GHC.Generics

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
  { versions :: Int
  , show_id :: Bool
  , show_tags :: Bool
  , hide_subjects :: Bool
  } deriving (Show, Generic)

-- | Default values for AssemblyOptions.
defaultAssemblyOptions :: AssemblyOptions
defaultAssemblyOptions = AssemblyOptions
  { versions = 1
  , show_id = False
  , show_tags = False
  , hide_subjects = False
  }

instance FromJSON AssemblyOptions where
  parseJSON = withObject "AssemblyOptions" $ \v -> AssemblyOptions
    <$> v .:? "versions" .!= versions defaultAssemblyOptions
    <*> v .:? "show_id" .!= show_id defaultAssemblyOptions
    <*> v .:? "show_tags" .!= show_tags defaultAssemblyOptions
    <*> v .:? "hide_subjects" .!= hide_subjects defaultAssemblyOptions

-- | Corresponds to the 'selection' section.
data Selection = Selection
  { include_tags :: [String]
  , exclude_tags :: [String]
  } deriving (Show, Generic)

-- | Default values for Selection.
defaultSelection :: Selection
defaultSelection = Selection { include_tags = [], exclude_tags = [] }

instance FromJSON Selection where
  parseJSON = withObject "Selection" $ \v -> Selection
    <$> v .:? "include_tags" .!= include_tags defaultSelection
    <*> v .:? "exclude_tags" .!= exclude_tags defaultSelection

-- | Corresponds to the 'content' section.
data Content = Content
  { instructions :: String
  } deriving (Show, Generic)

-- | Default values for Content.
defaultContent :: Content
defaultContent = Content { instructions = "" }

instance FromJSON Content where
  parseJSON = withObject "Content" $ \v -> Content
    <$> v .:? "instructions" .!= instructions defaultContent

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