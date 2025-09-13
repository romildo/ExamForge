module Main where

import ExamForge.Jupyter.Client (withKernel, executeCode, getVariableValue)
import System.ZMQ4.Monadic (liftIO)
import Text.Show.Pretty (pPrint)

pythonCode :: String
pythonCode = unlines
  [ "x = 40 + 22"
  , "y = 'hello from Python'"
  ]

haskellCode :: String
haskellCode = unlines
  [ "import Data.Char"
  , "x = 40 + 22"
  , "y = map toUpper \"hello from Haskell\""
  ]

-- Helper to define launch arguments for different kernels
ipythonArgs :: FilePath -> [String]
ipythonArgs connFile = ["kernel", "-f", connFile]

ihaskellArgs :: FilePath -> [String]
ihaskellArgs connFile = ["kernel", "--debug", connFile]

testKernelInteraction :: String -> (FilePath -> [String]) -> String -> IO ()
testKernelInteraction kernel argBuilder code =
  withKernel kernel argBuilder $ \session -> do
    liftIO $ putStrLn ("\n--- Testing Kernel: " ++ kernel ++ " ---")
    liftIO $ putStrLn "Executing initial code..."
    _ <- executeCode session code
    liftIO $ putStrLn "Code executed."

    liftIO $ putStrLn "\nRetrieving value of 'x'..."
    valX <- getVariableValue session "x"
    liftIO $ case valX of
      Just v -> putStrLn $ "SUCCESS! Value of 'x' is: " ++ v
      Nothing -> putStrLn "FAILURE to get 'x'."

    liftIO $ putStrLn "\nRetrieving value of 'y'..."
    valY <- getVariableValue session "y"
    liftIO $ case valY of
      Just v -> putStrLn $ "SUCCESS! Value of 'y' is: " ++ v
      Nothing -> putStrLn "FAILURE to get 'y'."

main :: IO ()
main = do
  putStrLn "--- Jupyter PoC Client (Language Agnostic) ---"
  testKernelInteraction "ipython" ipythonArgs pythonCode
  testKernelInteraction "ihaskell" ihaskellArgs haskellCode

