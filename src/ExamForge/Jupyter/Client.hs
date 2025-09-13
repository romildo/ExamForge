{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE DeriveGeneric         #-}

module ExamForge.Jupyter.Client
  ( JupyterSession
  , withKernel
  , executeCode
  , getVariableValue
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Exception (bracket)
import           Control.Monad (void, when)
import           Data.Aeson
import           Data.Aeson.Types (parseMaybe)
import           Data.Yaml (decodeFileEither, ParseException)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (isJust)
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

import           Crypto.Hash.Algorithms (SHA256)
import           Crypto.MAC.HMAC (HMAC, hmac)
import           Data.ByteArray.Encoding (convertToBase, Base(Base16))

data JupyterSession z = JupyterSession
  { jsSecretKey :: BC.ByteString
  , jsShellSock :: Socket z Dealer
  , jsIopubSock :: Socket z Sub
  , jsSessionId :: UUID.UUID
  }

-- MODIFIED: Now takes a function to build the kernel arguments.
withKernel :: String -> (FilePath -> [String]) -> (forall z. JupyterSession z -> ZMQ z a) -> IO a
withKernel kernelName argsBuilder action =
  bracket (startKernel kernelName argsBuilder) cleanupKernel $ \resources ->
    runZMQ $ do
      shellSock <- socket Dealer
      connect shellSock (formatAddr (krConnInfo resources) (shell_port (krConnInfo resources)))
      iopubSock <- socket Sub
      connect iopubSock (formatAddr (krConnInfo resources) (iopub_port (krConnInfo resources)))
      subscribe iopubSock ""
      sessionUUID <- liftIO nextRandom
      let secretKey = TE.encodeUtf8 . key . krConnInfo $ resources
      let session = JupyterSession secretKey shellSock iopubSock sessionUUID
      action session

executeCode :: JupyterSession z -> String -> ZMQ z (Maybe Value)
executeCode session code = do
  reqId <- sendExecuteRequest session code
  (reply, _) <- listenForReply session reqId
  return reply

getVariableValue :: JupyterSession z -> String -> ZMQ z (Maybe String)
getVariableValue session varName = do
  reqId <- sendExecuteRequest session varName
  (reply, _) <- listenForReply session reqId
  return (reply >>= extractExecuteResult)

data KernelResources = KernelResources
  { krConnFile   :: FilePath
  , krProcHandle :: ProcessHandle
  , krConnInfo   :: ConnectionInfo
  }

data ConnectionInfo = CI { ip :: String, transport :: String, shell_port :: Int, iopub_port :: Int, key :: T.Text } deriving (Show)
instance FromJSON ConnectionInfo where parseJSON = withObject "ConnectionInfo" $ \v -> CI <$> v .: "ip" <*> v .: "transport" <*> v .: "shell_port" <*> v .: "iopub_port" <*> v .: "key"
data Header = Header { msg_id :: T.Text, msg_type :: T.Text, session :: T.Text, username :: T.Text, version :: T.Text } deriving (Show, Generic)
instance ToJSON Header

-- MODIFIED: Now uses the argsBuilder function.
startKernel :: String -> (FilePath -> [String]) -> IO KernelResources
startKernel kernelName argsBuilder = do
  tmpDir <- getTemporaryDirectory
  let connf = tmpDir </> "jupyter_conn.json"
  fileExists <- doesFileExist connf
  when fileExists $ removeFile connf
  let args = argsBuilder connf
  (_, _, _, pHandle) <- createProcess (proc kernelName args)
  threadDelay 2000000
  cInfo <- either (fail . show) return =<< decodeFileEither connf
  return $ KernelResources { krConnFile = connf, krProcHandle = pHandle, krConnInfo = cInfo }

cleanupKernel :: KernelResources -> IO ()
cleanupKernel KernelResources{..} = do
  putStrLn "\n--- Shutting down kernel ---"
  terminateProcess krProcHandle
  threadDelay 500000
  fileExists <- doesFileExist krConnFile
  when fileExists $ removeFile krConnFile

createSignature :: BC.ByteString -> [BC.ByteString] -> BC.ByteString
createSignature key parts = convertToBase Base16 (hmac key (mconcat parts) :: HMAC SHA256)
sendMessage :: ToJSON a => JupyterSession z -> T.Text -> a -> ZMQ z UUID.UUID
sendMessage JupyterSession{..} msgType content = do
  reqId <- liftIO nextRandom
  let header = Header (UUID.toText reqId) msgType (UUID.toText jsSessionId) "haskell-client" "5.3"
  let headerBS = LBC.toStrict $ encode $ toJSON header; parentHeaderBS = "{}"; metadataBS = "{}"; contentBS = LBC.toStrict $ encode $ toJSON content
      signature = createSignature jsSecretKey [headerBS, parentHeaderBS, metadataBS, contentBS]
  sendMulti jsShellSock $ NE.fromList [ "<IDS|MSG>", signature, headerBS, parentHeaderBS, metadataBS, contentBS ]
  return reqId

sendExecuteRequest :: JupyterSession z -> String -> ZMQ z UUID.UUID
sendExecuteRequest session code =
  sendMessage session "execute_request" (object ["code" .= code, "silent" .= False, "store_history" .= False])
listenForReply :: forall a b z. (FromJSON a, IsString b, Eq b) => JupyterSession z -> UUID.UUID -> ZMQ z (Maybe a, b)
listenForReply JupyterSession{..} expectedMsgId = go Nothing
  where
    go capturedResult = do
      iopubMsgParts <- receiveMulti jsIopubSock
      let parentId = iopubExtractParentMsgId iopubMsgParts
      if parentId /= Just (UUID.toText expectedMsgId)
        then go capturedResult
        else do
          let status = iopubExtractExecutionStatus iopubMsgParts
          let msgType = iopubExtractMsgType iopubMsgParts
          let newResult = if msgType == Just "execute_result" then iopubExtractContent iopubMsgParts else capturedResult
          if status == Just "idle"
            then do
              void $ receiveMulti jsShellSock
              return (newResult, "idle")
            else go newResult

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
formatAddr :: ConnectionInfo -> Int -> String
formatAddr ci port = transport ci ++ "://" ++ ip ci ++ ":" ++ show port

