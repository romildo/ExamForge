{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

import           Control.Concurrent (threadDelay)
import           Control.Exception (bracket)
import           Control.Monad (forever, forM_, void, when)
import           Data.Aeson
import           Data.Aeson.Types (parseMaybe, Parser)
import           Data.Yaml (decodeFileEither)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromJust, isJust)
import           Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import           Data.UUID.V4 (nextRandom)
import           GHC.Generics (Generic)
import           System.Directory (getTemporaryDirectory, removeFile, doesFileExist)
import           System.FilePath ((</>))
import           System.Process
import           System.ZMQ4.Monadic

-- Crypto Imports
import           Crypto.Hash.Algorithms (SHA256)
import           Crypto.MAC.HMAC (HMAC, hmac)
import           Data.ByteArray.Encoding (convertToBase, Base(Base16))

-- A record to hold all kernel resources for clean management
data KernelResources = KernelResources
  { krConnFile   :: FilePath
  , krConnInfo   :: ConnectionInfo
  , krProcHandle :: ProcessHandle
  }

data ConnectionInfo = ConnectionInfo { ip :: String, transport :: String, shell_port :: Int, iopub_port :: Int, key :: T.Text } deriving (Show)
instance FromJSON ConnectionInfo where
    parseJSON = withObject "ConnectionInfo" $ \v -> ConnectionInfo <$> v .: "ip" <*> v .: "transport" <*> v .: "shell_port" <*> v .: "iopub_port" <*> v .: "key"

data Header = Header { msg_id :: T.Text, msg_type :: T.Text, session :: T.Text, username :: T.Text, version :: T.Text } deriving (Show, Generic)
instance ToJSON Header

main :: IO ()
main = do
  putStrLn "--- Jupyter PoC Client ---"
  bracket launchKernel cleanupKernel $ \resources -> do
    let shellAddr = formatAddr (krConnInfo resources) (shell_port (krConnInfo resources))
    let iopubAddr = formatAddr (krConnInfo resources) (iopub_port (krConnInfo resources))
    let secretKey = TE.encodeUtf8 . key . krConnInfo $ resources

    runZMQ $ do
      shellSock <- socket Dealer
      connect shellSock shellAddr
      liftIO $ putStrLn $ "Connected to SHELL port at " ++ shellAddr

      iopubSock <- socket Sub
      connect iopubSock iopubAddr
      subscribe iopubSock ""
      liftIO $ putStrLn $ "Connected to IOPub port at " ++ iopubAddr

      sessionUUID <- liftIO nextRandom
      let codeToRun = "x = 42\ny = 'hello from Haskell'"
      liftIO $ putStrLn $ "\nSending code to execute:\n" ++ codeToRun

      execReqId <- sendExecuteRequest secretKey shellSock sessionUUID codeToRun
      liftIO $ putStrLn "Waiting for execution reply..."
      (execReply :: Maybe Value, execStatus :: String) <- listenForReply shellSock iopubSock execReqId

      liftIO $ putStrLn $ "Execution status: " ++ execStatus

      liftIO $ putStrLn "\nSending execute request to get value of 'x'..."
      valueReqId <- sendExecuteRequest secretKey shellSock sessionUUID "x"
      liftIO $ putStrLn "Waiting for value reply..."
      (valueReply :: Maybe Value, _) <- listenForReply shellSock iopubSock valueReqId

      let varValue = valueReply >>= extractExecuteResult
      liftIO $ case varValue of
        Just val -> putStrLn $ "\nSUCCESS! Value of 'x' is: " ++ val
        Nothing  -> putStrLn "\nFAILURE! Could not retrieve value of 'x'."

launchKernel :: IO KernelResources
launchKernel = do
  tmpDir <- getTemporaryDirectory
  let connf = tmpDir </> "jupyter_conn.json"
  fileExists <- doesFileExist connf
  when fileExists $ removeFile connf

  let args = ["kernel", "-f", connf]
  (_, _, _, pHandle) <- createProcess (proc "ipython" args)
  putStrLn "Waiting for kernel to start..."
  threadDelay 2000000 -- Wait 2s for the file to be written

  putStrLn $ "Kernel launched. Connection file at: " ++ connf
  cInfo <- either (fail . show) return =<< decodeFileEither connf
  return $ KernelResources { krConnFile = connf, krConnInfo = cInfo, krProcHandle = pHandle }

cleanupKernel :: KernelResources -> IO ()
cleanupKernel resources = do
  putStrLn "\nCleaning up..."
  terminateProcess (krProcHandle resources)
  threadDelay 500000
  fileExists <- doesFileExist (krConnFile resources)
  when fileExists $ removeFile (krConnFile resources)
  putStrLn "Kernel terminated and connection file removed."

-- CORRECTED: The stateful listening loop
listenForReply :: (FromJSON a, IsString b, Eq b) => Socket z Dealer -> Socket z Sub -> T.Text -> ZMQ z (Maybe a, b)
listenForReply shellSock iopubSock expectedMsgId = go Nothing
  where
    go capturedResult = do
      iopubMsgParts <- receiveMulti iopubSock
      let parentId = iopubExtractParentMsgId iopubMsgParts
      
      if parentId /= Just expectedMsgId
        then go capturedResult -- Not our message, keep listening
        else do
          let status = iopubExtractExecutionStatus iopubMsgParts
          let msgType = iopubExtractMsgType iopubMsgParts
          
          -- Capture the result when we see it
          let newResult = if msgType == Just "execute_result"
                            then iopubExtractContent iopubMsgParts
                            else capturedResult

          -- If we are idle, we are done. Return what we found.
          if status == Just "idle"
            then do
              -- We still need to consume the final reply from the SHELL socket
              void $ receiveMulti shellSock
              return (newResult, "idle")
            else go newResult -- Not idle yet, keep listening with updated result

-- Message Parsing Helpers
iopubExtractParentMsgId :: [BC.ByteString] -> Maybe T.Text
iopubExtractParentMsgId parts = if length parts > 4 then decodeStrict (parts !! 4) >>= parseMaybe (.: "msg_id") else Nothing

iopubExtractMsgType :: [BC.ByteString] -> Maybe String
iopubExtractMsgType parts = if length parts > 3 then decodeStrict (parts !! 3) >>= parseMaybe (.: "msg_type") else Nothing

iopubExtractContent :: FromJSON a => [BC.ByteString] -> Maybe a
iopubExtractContent parts = if length parts > 6 then decodeStrict (parts !! 6) else Nothing

iopubExtractExecutionStatus :: [BC.ByteString] -> Maybe String
iopubExtractExecutionStatus parts = if length parts > 6 then decodeStrict (parts !! 6) >>= parseMaybe (.: "execution_state") else Nothing

extractExecuteResult :: Value -> Maybe String
extractExecuteResult = parseMaybe $ withObject "content" $ \c -> do
  d <- c .: "data"
  d .: "text/plain"

-- (The rest of the file is unchanged)
createSignature :: BC.ByteString -> [BC.ByteString] -> BC.ByteString
createSignature key parts = convertToBase Base16 (hmac key (mconcat parts) :: HMAC SHA256)
sendMessage :: ToJSON a => BC.ByteString -> Socket z Dealer -> Header -> a -> ZMQ z ()
sendMessage key sock header content =
  let headerBS = LBC.toStrict $ encode $ toJSON header; parentHeaderBS = "{}"; metadataBS = "{}"; contentBS = LBC.toStrict $ encode $ toJSON content
      signature = createSignature key [headerBS, parentHeaderBS, metadataBS, contentBS]
  in sendMulti sock $ NE.fromList [ "<IDS|MSG>", signature, headerBS, parentHeaderBS, metadataBS, contentBS ]
sendExecuteRequest :: BC.ByteString -> Socket z Dealer -> UUID.UUID -> String -> ZMQ z T.Text
sendExecuteRequest key sock sessionUUID code = do
  reqId <- liftIO nextRandom
  let header = createHeader reqId "execute_request" sessionUUID
  let content = object ["code" .= code, "silent" .= False]
  sendMessage key sock header content; return (UUID.toText reqId)
createHeader :: UUID.UUID -> T.Text -> UUID.UUID -> Header
createHeader msgId msgType sId = Header (UUID.toText msgId) msgType (UUID.toText sId) "haskell-client" "5.3"
formatAddr :: ConnectionInfo -> Int -> String
formatAddr ci port = transport ci ++ "://" ++ ip ci ++ ":" ++ show port