module Main where

import ExamForge.Jupyter.Client (withKernel, executeCode, getVariableValue)
import System.ZMQ4.Monadic (liftIO)
import Control.Exception (try, SomeException)
import Control.Monad (forM_)

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

cCode :: String
cCode = unlines
  [ "int x = 40 + 22;"
  , "const int n = 100;"
  , "char y[n] = \"\";"
  , "sprintf(y, \"The value is %d\", n);"
  ]

ipythonArgs :: FilePath -> [String]
ipythonArgs connFile = ["kernel", "-f", connFile]

ihaskellArgs :: FilePath -> [String]
ihaskellArgs connFile = ["kernel", connFile]

clangArgs :: FilePath -> [String]
clangArgs connFile = ["kernel", connFile]

xeusArgs :: FilePath -> [String]
xeusArgs connFile = ["kernel", "-f", connFile, "-std=c++17"]

cArgs :: FilePath -> [String]
cArgs connFile = ["kernel", "-m", "jupyter_c_kernel", "-f", connFile]

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
  forM_ tests $ \(kernel, args, code) -> do
    result <- try (testKernelInteraction kernel args code) :: IO (Either SomeException ())
    case result of
      Left e -> putStrLn $ "\n" ++ kernel ++ " test finished with an exception (this is often normal on shutdown): " ++ show e
      Right () -> return ()
  where
    tests =
      [ ("ipython", ipythonArgs, pythonCode)
      , ("ihaskell", ihaskellArgs, haskellCode)
      -- , ("clang-repl-kernel-start", clangArgs, cCode)
      , ("python3", cArgs, cCode)
      , ("xcpp", xeusArgs, cCode)
      ]
