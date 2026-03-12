{-# LANGUAGE OverloadedStrings #-}

module ExamForge.Orchestrator 
  ( evaluateQuestion 
  ) where

import ExamForge.ExamConfig (EvaluatorConfig(..))
import ExamForge.IR (ExpandedQuestion)
import System.Process (readCreateProcessWithExitCode, shell)
import System.Exit (ExitCode(..))
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson (eitherDecode)

-- | Replaces placeholders like "%f" with the actual file paths.
replacePlaceholder :: String -> String -> String -> String
replacePlaceholder target replacement = 
    T.unpack . T.replace (T.pack target) (T.pack replacement) . T.pack

-- | Takes the evaluator config and the generated source code, runs it, 
--   and returns either an Error message or the raw JSON string.
executeScript :: EvaluatorConfig -> String -> IO (Either String String)
executeScript config sourceCode = 
  withSystemTempDirectory "examforge_workspace" $ \workspace -> do
    
    -- Determine the file extension (e.g., ".c", ".py", or "")
    let ext = case extension config of
                Just e  -> "." ++ e
                Nothing -> ""
                
    -- Append the extension to the source file name
    let srcPath = workspace </> ("eval_script" ++ ext)
    let exePath = workspace </> "eval_out"
    
    -- Write the generated source code to the temp file
    writeFile srcPath sourceCode

    -- Handle the optional BUILD step (for compiled languages like C/C++)
    buildResult <- case build config of
      Nothing -> return (Right ()) 
      Just buildCmdRaw -> do
        let buildCmd = replacePlaceholder "%e" exePath $ 
                       replacePlaceholder "%f" srcPath buildCmdRaw
        
        (exit, _, err) <- readCreateProcessWithExitCode (shell buildCmd) ""
        case exit of
          ExitSuccess   -> return (Right ())
          ExitFailure _ -> return (Left $ "Build failed:\n" ++ err)

    -- Handle the RUN step
    case buildResult of
      Left buildError -> return (Left buildError)
      Right () -> do
        let runCmd = replacePlaceholder "%e" exePath $ 
                     replacePlaceholder "%f" srcPath (run config)
        
        (exit, out, err) <- readCreateProcessWithExitCode (shell runCmd) ""
        case exit of
          ExitSuccess   -> return (Right out)
          ExitFailure _ -> return (Left $ "Execution failed:\n" ++ err ++ "\nStdout was:\n" ++ out)

-- | The main entry point for Phase 1. 
--   It runs the script and parses the resulting JSON into the Haskell IR.
evaluateQuestion :: EvaluatorConfig -> String -> IO (Either String ExpandedQuestion)
evaluateQuestion config sourceCode = do
  execResult <- executeScript config sourceCode
  case execResult of
    Left err -> return (Left err)
    Right jsonOutput -> 
      -- Properly encode the Unicode String into UTF-8 bytes
      let utf8Bytes = BSL.fromStrict (TE.encodeUtf8 (T.pack jsonOutput))
      in case eitherDecode utf8Bytes of
        Left parseErr -> return $ Left ("JSON Parse Error: " ++ parseErr ++ "\nOutput was:\n" ++ jsonOutput)
        Right parsedIR -> return (Right parsedIR)
