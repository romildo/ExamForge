{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Module      : ExamForge.Jupyter.Client
-- Description : A high-level client for interacting with Jupyter kernels.
--
-- This module provides a safe, high-level API for starting, managing,
-- and communicating with Jupyter kernels (such as IPython or IHaskell).
-- It handles the complexities of the ZeroMQ-based wire protocol, including
-- process management, message signing, and asynchronous reply handling.
module ExamForge.Jupyter.Client
  ( -- * Session Management
    JupyterSession
  , withKernel

    -- * Kernel Interaction
  , executeCode
  , getVariableValue
  ) where

import Control.Applicative ((<|>))
import           Control.Concurrent (threadDelay)
import           Control.Exception (bracket, catch, SomeException)
import           Control.Monad (void, when, forM)
import           Data.Aeson
import qualified Data.Aeson as A
import           Data.Aeson.Types (Parser, Value(..), parseMaybe)
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

-- Network import for finding free ports
import qualified Network.Socket as Net
import           Network.Socket (withSocketsDo)


-- Crypto Imports
import           Crypto.Hash.Algorithms (SHA256)
import           Crypto.MAC.HMAC (HMAC, hmac)
import           Data.ByteArray.Encoding (convertToBase, Base(Base16))

-- =============================================================================
-- == Public Data Types & API
-- =============================================================================

-- | A handle to an active Jupyter kernel session. This is an opaque type
-- that holds the state for communicating with a specific kernel, including
-- ZMQ sockets and cryptographic keys. It is polymorphic over the ZMQ context 'z'.
data JupyterSession z = JupyterSession
  { jsSecretKey :: BC.ByteString
  , jsShellSock :: Socket z Dealer
  , jsControlSock :: Socket z Dealer
  , jsIopubSock :: Socket z Sub
  , jsSessionId :: UUID.UUID
  }


-- | Safely starts a kernel, runs the given ZMQ actions with the session,
-- | and guarantees the kernel process is shut down afterward, even if errors occur.
-- | This is the recommended entry point for using the client.
-- |
-- | @param kernelName The name of the kernel executable (e.g., "ipython", "ihaskell").
-- | @param argsBuilder A function that takes the temporary connection file path and returns the list of command-line arguments to start the kernel.
-- | @param action A ZMQ computation to be run with the active session.
withKernel :: String -> (FilePath -> [String]) -> (forall z. JupyterSession z -> ZMQ z a) -> IO a
withKernel kernelName argsBuilder action =
  bracket (startKernel kernelName argsBuilder) cleanupKernel $ \resources ->
    runZMQ $ do
      shellSock <- socket Dealer
      connect shellSock (formatAddr (krConnInfo resources) (shell_port (krConnInfo resources)))
      iopubSock <- socket Sub
      connect iopubSock (formatAddr (krConnInfo resources) (iopub_port (krConnInfo resources)))
      subscribe iopubSock ""
      controlSock <- socket Dealer
      connect controlSock (formatAddr (krConnInfo resources) (control_port (krConnInfo resources)))
      sessionUUID <- liftIO nextRandom
      let secretKey = TE.encodeUtf8 . key . krConnInfo $ resources
      let session = JupyterSession secretKey shellSock controlSock iopubSock sessionUUID
      action session

-- | Executes a block of code in the kernel, primarily for its side effects (e.g., defining variables).
-- | This function waits until the kernel reports that it is idle, ensuring the code has finished executing.
executeCode :: JupyterSession z -> String -> ZMQ z ()
executeCode session code = do
  reqId <- sendExecuteRequest session code
  -- We listen for the reply to ensure completion, but discard the result.
  -- A type annotation is needed to resolve the ambiguity of the polymorphic result.
  (_ :: Maybe Value, _ :: String) <- listenForReply session reqId
  return ()

-- Fix getVariableValue to use the unified path

-- | Retrieves the string representation of a variable from the kernel.
-- | This uses the 'user_expressions' feature of the Jupyter protocol, which is
-- | the standard, language-agnostic way to inspect variable values.
getVariableValue :: JupyterSession z -> String -> ZMQ z (Maybe String)
getVariableValue session varName = do
  -- Try user_expressions first (works in IPython)
  let exprs = object ["var" .= String (T.pack varName)]
  reqId1 <- sendExecuteRequestWith session "" exprs
  (reply :: Maybe Value, _) <- listenForReply session reqId1
  case (reply >>= extractUserExpressionResult) of
    Just v  -> pure (Just v)
    Nothing -> do
      -- Fallback for kernels (like IHaskell) that ignore user_expressions:
      -- send the variable name as code and collect text/plain from IOPub.
      reqId2 <- sendExecuteRequest session varName
      listenForPlainResult session reqId2

-- =============================================================================
-- == Internal Implementation Details
-- =============================================================================

-- === New helper ===
data WireFrames = WireFrames
  { wfSig     :: BC.ByteString
  , wfHeader  :: BC.ByteString
  , wfParent  :: BC.ByteString
  , wfMeta    :: BC.ByteString
  , wfContent :: BC.ByteString
  , wfBuffers :: [BC.ByteString]
  }

dropToFrames :: [BC.ByteString] -> Maybe WireFrames
dropToFrames xs =
  case dropWhile (/= "<IDS|MSG>") xs of
    (_delim:sig:hdr:parent:meta:content:rest) ->
      Just (WireFrames sig hdr parent meta content rest)
    _ -> Nothing



data KernelResources = KernelResources
  { krConnFile   :: FilePath
  , krProcHandle :: ProcessHandle
  , krConnInfo   :: ConnectionInfo
  }

data ConnectionInfo = CI { ip :: String, transport :: String, shell_port :: Int, iopub_port :: Int, stdin_port :: Int, control_port :: Int, hb_port :: Int, key :: T.Text, signature_scheme :: String } deriving (Show, Generic)
instance ToJSON ConnectionInfo
instance FromJSON ConnectionInfo

data Header = Header { msg_id :: T.Text, msg_type :: T.Text, session :: T.Text, username :: T.Text, version :: T.Text } deriving (Show, Generic)
instance ToJSON Header

findFreePort :: IO Int
findFreePort = withSocketsDo $ do
  addr <- resolve "127.0.0.1" "0"
  sock <- Net.socket (Net.addrFamily addr) (Net.addrSocketType addr) (Net.addrProtocol addr)
  Net.bind sock (Net.addrAddress addr)
  port <- Net.socketPort sock
  Net.close sock
  return (fromIntegral port)
  where
    resolve host port = do
        let hints = Net.defaultHints { Net.addrSocketType = Net.Stream }
        head <$> Net.getAddrInfo (Just hints) (Just host) (Just port)

startKernel :: String -> (FilePath -> [String]) -> IO KernelResources
startKernel kernelName argsBuilder = do
  tmpDir <- getTemporaryDirectory
  let connf = tmpDir </> "jupyter_conn.json"
  [s_port, io_port, si_port, c_port, hb_port] <- forM [1..5] (const findFreePort)
  uuidKey <- UUID.toText <$> nextRandom
  let cInfo = CI "127.0.0.1" "tcp" s_port io_port si_port c_port hb_port uuidKey "hmac-sha256"
  encodeFile connf cInfo
  let args = argsBuilder connf
  (_, _, _, pHandle) <- createProcess (proc kernelName args)
  threadDelay 1000000
  return $ KernelResources { krConnFile = connf, krProcHandle = pHandle, krConnInfo = cInfo }

shutdownKernel :: JupyterSession z -> ZMQ z ()
shutdownKernel JupyterSession{..} =
  sendMessageOn jsControlSock jsSecretKey jsSessionId "shutdown_request" (object ["restart" .= False])

cleanupKernel :: KernelResources -> IO ()
cleanupKernel kr@KernelResources{..} = do
  putStrLn "\n--- Shutting down kernel ---"

  -- 1) Ask nicely (Jupyter way). Ignore any exceptions here.
  _ <- (sendShutdownIO kr >> pure ()) `catch` \(_ :: SomeException) -> pure ()

  -- 2) Give the kernel a brief moment to exit on its own.
  threadDelay 300000  -- 300 ms

  -- 3) If it's still alive, fall back to SIGTERM (terminateProcess).
  mExit <- getProcessExitCode krProcHandle
  case mExit of
    Nothing -> do
      terminateProcess krProcHandle
      threadDelay 500000
    Just _  -> pure ()

  -- 4) Clean up the ephemeral connection file.
  fileExists <- doesFileExist krConnFile
  when fileExists $ removeFile krConnFile


-- Graceful shutdown via control channel (no refactor needed)
sendShutdownIO :: KernelResources -> IO ()
sendShutdownIO KernelResources{ krConnInfo = ci } = runZMQ $ do
  controlSock <- socket Dealer
  connect controlSock (formatAddr ci (control_port ci))

  -- Build a valid Jupyter message
  sessionUUID <- liftIO nextRandom
  reqUUID     <- liftIO nextRandom
  let secretKey     = TE.encodeUtf8 (key ci)
      header        = Header (UUID.toText reqUUID) "shutdown_request"
                              (UUID.toText sessionUUID) "haskell-client" "5.3"
      headerBS      = LBC.toStrict (encode (toJSON header))
      parentHeaderBS= "{}"
      metadataBS    = "{}"
      contentBS     = LBC.toStrict (encode (object ["restart" .= False]))
      signature     = createSignature secretKey [headerBS, parentHeaderBS, metadataBS, contentBS]

  -- Fire-and-forget (kernels reply on control; we don't strictly need to read it)
  sendMulti controlSock $
    NE.fromList [ "<IDS|MSG>", signature, headerBS, parentHeaderBS, metadataBS, contentBS ]


createSignature :: BC.ByteString -> [BC.ByteString] -> BC.ByteString
createSignature key parts = convertToBase Base16 (hmac key (mconcat parts) :: HMAC SHA256)

sendMessageOn :: ToJSON a => Socket z Dealer -> BC.ByteString -> UUID.UUID -> T.Text -> a -> ZMQ z ()
sendMessageOn sock key sessionId msgType content = do
  reqId <- liftIO nextRandom
  let header = Header (UUID.toText reqId) msgType (UUID.toText sessionId) "haskell-client" "5.3"
      headerBS = LBC.toStrict $ encode $ toJSON header
      parentHeaderBS = "{}"; metadataBS = "{}"
      contentBS = LBC.toStrict $ encode $ toJSON content
      signature = createSignature key [headerBS, parentHeaderBS, metadataBS, contentBS]
  sendMulti sock $ NE.fromList ["<IDS|MSG>", signature, headerBS, parentHeaderBS, metadataBS, contentBS]

-- keep a convenience wrapper for shell
sendMessage :: ToJSON a => JupyterSession z -> T.Text -> a -> ZMQ z UUID.UUID
sendMessage JupyterSession{..} msgType content = do
  reqId <- liftIO nextRandom
  let header = Header (UUID.toText reqId) msgType (UUID.toText jsSessionId) "haskell-client" "5.3"
      headerBS = LBC.toStrict $ encode $ toJSON header
      parentHeaderBS = "{}"; metadataBS = "{}"
      contentBS = LBC.toStrict $ encode $ toJSON content
      signature = createSignature jsSecretKey [headerBS, parentHeaderBS, metadataBS, contentBS]
  sendMulti jsShellSock $ NE.fromList ["<IDS|MSG>", signature, headerBS, parentHeaderBS, metadataBS, contentBS]
  pure reqId

-- New unified sender
sendExecuteRequestWith
  :: JupyterSession z
  -> String                 -- ^ code
  -> Value                  -- ^ user_expressions (object)
  -> ZMQ z UUID.UUID
sendExecuteRequestWith session code userExprs =
  let content = object
        [ "code"           .= code
        , "silent"         .= False
        , "store_history"  .= True
        , "allow_stdin"    .= False
        , "stop_on_error"  .= True
        , "user_expressions" .= userExprs
        ]
  in sendMessage session "execute_request" content

-- Keep this for the "side-effects only" path; just delegate:
sendExecuteRequest :: JupyterSession z -> String -> ZMQ z UUID.UUID
sendExecuteRequest session code =
  sendExecuteRequestWith session code (object [])

listenForReply :: forall a b z. (FromJSON a, IsString b, Eq b) => JupyterSession z -> UUID.UUID -> ZMQ z (Maybe a, b)
listenForReply JupyterSession{..} expectedMsgId = go
  where
    go = do
      iopubMsgParts <- receiveMulti jsIopubSock
      let parentId = iopubExtractParentMsgId iopubMsgParts
      if parentId /= Just (UUID.toText expectedMsgId)
        then go
        else do
          let status = iopubExtractExecutionStatus iopubMsgParts
          if status == Just "idle"
            then do
              shellMsgParts <- receiveMulti jsShellSock
              -- DEBUG: peek at the shell frames we just received
              -- liftIO $ putStrLn ("[shell] " <> show (map (BC.take 120) shellMsgParts))
              return (shellExtractContent shellMsgParts, "idle")
            else go

listenForPlainResult :: JupyterSession z -> UUID.UUID -> ZMQ z (Maybe String)
listenForPlainResult JupyterSession{..} expectedMsgId = go Nothing
  where
    go acc = do
      parts <- receiveMulti jsIopubSock
      let parentId = iopubExtractParentMsgId parts
      if parentId /= Just (UUID.toText expectedMsgId)
        then go acc
        else do
          let acc' = iopubExtractTextPlain parts <|> acc
          case iopubExtractExecutionStatus parts of
            Just "idle" -> do
              -- drain the corresponding shell reply (we don't need its content)
              _ <- receiveMulti jsShellSock
              pure acc'
            _ -> go acc'


iopubExtractParentMsgId :: [BC.ByteString] -> Maybe T.Text
iopubExtractParentMsgId parts = do
  WireFrames{wfParent} <- dropToFrames parts
  A.decodeStrict wfParent >>= parseMaybe (.: "msg_id")

iopubExtractExecutionStatus :: [BC.ByteString] -> Maybe String
iopubExtractExecutionStatus parts = do
  WireFrames{wfHeader,wfContent} <- dropToFrames parts
  msgType <- (A.decodeStrict wfHeader :: Maybe A.Object)
             >>= parseMaybe (\o -> o .: "msg_type" :: Parser String)
  if msgType == "status"
     then (A.decodeStrict wfContent :: Maybe A.Object)
           >>= parseMaybe (\o -> o .: "execution_state" :: Parser String)
     else Nothing

shellExtractContent :: A.FromJSON a => [BC.ByteString] -> Maybe a
shellExtractContent parts = do
  WireFrames{wfContent} <- dropToFrames parts
  A.decodeStrict wfContent

-- What kind of IOPub message is this?
iopubMsgType :: [BC.ByteString] -> Maybe String
iopubMsgType parts = do
  WireFrames{wfHeader} <- dropToFrames parts
  (A.decodeStrict wfHeader :: Maybe A.Object)
    >>= parseMaybe (\o -> o .: "msg_type" :: Parser String)

-- Extract plain text from execute_result / display_data / stream
iopubExtractTextPlain :: [BC.ByteString] -> Maybe String
iopubExtractTextPlain parts = do
  WireFrames{wfContent} <- dropToFrames parts
  typ <- iopubMsgType parts
  case typ of
    "execute_result" -> do
      obj <- (A.decodeStrict wfContent :: Maybe A.Object)
      dataObj <- parseMaybe (\o -> o .: "data") obj
      parseMaybe (\d -> d .: "text/plain" :: Parser String) dataObj
    "display_data" -> do
      obj <- (A.decodeStrict wfContent :: Maybe A.Object)
      dataObj <- parseMaybe (\o -> o .: "data") obj
      parseMaybe (\d -> d .: "text/plain" :: Parser String) dataObj
    "stream" -> do
      obj <- (A.decodeStrict wfContent :: Maybe A.Object)
      -- only stdout is useful here
      name <- parseMaybe (\o -> o .: "name" :: Parser String) obj
      if name == "stdout"
        then parseMaybe (\o -> o .: "text" :: Parser String) obj
        else Nothing
    _ -> Nothing


extractUserExpressionResult :: Value -> Maybe String
extractUserExpressionResult = parseMaybe $ withObject "content" $ \c -> do
  userExprs <- c .: "user_expressions"
  var <- userExprs .: "var"
  dataObj <- var .: "data"
  dataObj .: "text/plain"

formatAddr :: ConnectionInfo -> Int -> String
formatAddr ci port = transport ci ++ "://" ++ ip ci ++ ":" ++ show port

