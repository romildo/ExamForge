-- File: app/MockGen.hs
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (nub)
import Data.Aeson (Value(..), object, (.=), toJSON)
import qualified Data.Yaml as Yaml
import Options.Applicative
import System.Random (StdGen, mkStdGen, randomR)
import Text.Printf (printf)

-- | Data type representing our parsed CLI options
data MockOptions = MockOptions
  { optBankOut      :: FilePath
  , optConfigOut    :: FilePath
  , optNumQuestions :: Int
  , optNumGroups    :: Int
  , optGroupPrefix  :: String
  , optVersions     :: Int
  , optMaxPerGroup  :: Int
  , optSeed         :: Int
  } deriving Show

-- | The optparse-applicative parser definition
optionsParser :: Parser MockOptions
optionsParser = MockOptions
  <$> strOption
      ( long "bank-out"
     <> metavar "FILE"
     <> help "Output path for the generated question bank YAML (e.g., questions/mock.yml)" )
  <*> strOption
      ( long "config-out"
     <> metavar "FILE"
     <> help "Output path for the generated exam config YAML (e.g., configs/mock.yml)" )
  <*> option auto
      ( long "num-questions"
     <> short 'q'
     <> metavar "INT"
     <> help "Total number of questions to generate in the bank" )
  <*> option auto
      ( long "num-groups"
     <> short 'g'
     <> metavar "INT"
     <> help "Total number of available semantic group tags in the universe" )
  <*> strOption
      ( long "group-prefix"
     <> short 'p'
     <> metavar "STRING"
     <> value "grupo-"
     <> showDefault
     <> help "Prefix for semantic group tags" )
  <*> option auto
      ( long "versions"
     <> short 'v'
     <> metavar "INT"
     <> help "Number of exam variations to specify in the config" )
  <*> option auto
      ( long "max-per-group"
     <> short 'm'
     <> metavar "INT"
     <> value 1
     <> showDefault
     <> help "Maximum questions per semantic group per variant" )
  <*> option auto
      ( long "seed"
     <> short 's'
     <> metavar "INT"
     <> value 42
     <> showDefault
     <> help "Random seed for deterministic generation" )

-- | Main entry point
main :: IO ()
main = do
  opts <- execParser optsInfo
  let gen = mkStdGen (optSeed opts)
  
  putStrLn $ "Generating mock question bank with " ++ show (optNumQuestions opts) ++ " questions..."
  let (bankValue, _) = generateBank gen (optGroupPrefix opts) (optNumQuestions opts) (optNumGroups opts)
  Yaml.encodeFile (optBankOut opts) bankValue
  putStrLn $ "Wrote question bank to: " ++ optBankOut opts

  putStrLn "Generating mock exam config..."
  let configValue = generateConfig (optBankOut opts) (optGroupPrefix opts) (optVersions opts) (optMaxPerGroup opts) (optSeed opts)
  Yaml.encodeFile (optConfigOut opts) configValue
  putStrLn $ "Wrote exam config to: " ++ optConfigOut opts

  where
    optsInfo = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Generates mock question banks and exam configs to stress-test the ExamForge selection and rotation algorithms."
     <> header "examforge-mockgen - Testing utility for ExamForge" )

-- | Picks a random sublist of elements (0 to 2 tags per question for varied density).
pickRandomTags :: StdGen -> [String] -> (StdGen, [String])
pickRandomTags gen universe =
  let (numTags, gen1) = randomR (0, 2 :: Int) gen 
      pick _ (g, acc) = 
        let (idx, g') = randomR (0, length universe - 1) g
        in (g', universe !! idx : acc)
      (genFinal, tags) = foldr pick (gen1, []) [1..numTags]
  in (genFinal, tags)

-- | Generates the YAML Value for the question bank.
generateBank :: StdGen -> String -> Int -> Int -> (Value, StdGen)
generateBank gen prefix numQs numGrps =
  let
    universe = [prefix ++ show i | i <- [1..numGrps]]
    
    mkQuestion (g, idx) =
      let (g', tags) = pickRandomTags g universe
          qObj = object
            [ "id" .= (printf "mock-q-%04d" (idx :: Int) :: String)
            , "title" .= (printf "Mock Question %d" idx :: String)
            , "format" .= ("latex" :: String)
            , "selection_type" .= ("any" :: String)
            , "tags" .= nub tags
            , "question" .= ("What is the mock answer for Q" ++ show idx ++ "?" :: String)
            , "answers" .= 
                [ object ["correct" .= ("True" :: String)]
                , object ["incorrect" .= ("False" :: String)]
                ]
            ]
      in (g', qObj)
      
    (finalGen, qObjs) = foldl (\(g, acc) idx -> let (g', q) = mkQuestion (g, idx) in (g', q : acc)) (gen, []) [1..numQs]
  in
    (toJSON (reverse qObjs), finalGen)

-- | Generates the YAML Value for the exam config.
generateConfig :: FilePath -> String -> Int -> Int -> Int -> Value
generateConfig bankFile prefix numVersions maxPerGroup examSeed =
  object
    [ "header" .= object
        [ "institution" .= ("Mock University" :: String)
        , "course" .= ("MOCK 101: Algorithmic Testing" :: String)
        , "professor" .= ("Dr. Test" :: String)
        , "semester" .= ("2026/1" :: String)
        , "title" .= ("Load Test Exam" :: String)
        ]
    , "question_banks" .= [bankFile]
    , "assembly_options" .= object
        [ "versions" .= numVersions
        , "show_id" .= True
        , "show_tags" .= True
        , "shuffle_questions" .= True
        , "registration_digits" .= (7 :: Int)
        , "seed" .= examSeed
        ]
    , "selection" .= object
        [ "semantic_groups" .= 
            [ object
                [ "pattern" .= ("^" ++ prefix ++ ".*$")
                , "maximum" .= maxPerGroup
                ]
            ]
        ]
    ]
