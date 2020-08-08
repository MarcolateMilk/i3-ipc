
{-# LANGUAGE OverloadedStrings #-}

module Network.I3IPC.Internal where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy   ( ByteString )
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import Control.Exception      ( bracketOnError )
import Network.Socket
import Network.Socket.ByteString.Lazy
import System.Process         ( readProcess )
import Text.Printf            ( printf )

import qualified Data.ByteString.Lazy       as W
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Network.Socket             as S




-- -----------------------------------------------------------------------------
--
-- * Connection
--
-- -----------------------------------------------------------------------------


-- | Abstraction over an active connection to i3.
newtype Connection

  = Connection                  Socket

  deriving ( Eq )


-- -----------------------------------------------------------------------------


-- | Establish a connection to i3.
connect :: IO Connection
connect
  = bracketOnError (socket AF_UNIX Stream 0) S.close $ \ sock -> do
    path <- socketPath
    S.connect sock $ SockAddrUnix path
    return $ Connection sock


-- | Close the connection.
close :: Connection -> IO ()
close (Connection sock)
  = gracefulClose sock 1000


-- | Get the socket path from i3.
socketPath :: IO String
socketPath
  = init <$> readProcess "i3" [ "--get-socketpath" ] []




-- -----------------------------------------------------------------------------
--
-- * Low-level messaging
--
-- -----------------------------------------------------------------------------


data MessageType

  = RUN_COMMAND
    -- ^ Run the payload as an i3 command (like the commands you can bind to keys).

  | GET_WORKSPACES
    -- ^ Get the list of current workspaces.

  | SUBSCRIBE
    -- ^ Subscribe to the event types specified in the message payload.

  | GET_OUTPUTS
    -- ^ Get the list of current outputs.

  | GET_TREE
    -- ^ Get the i3 layout tree.

  | GET_MARKS
    -- ^ Get the names of all currently set marks.

  | GET_BAR_CONFIG
    -- ^ Get the specified bar configuration or the names of all bar configurations.

  | GET_VERSION
    -- ^ Get the i3 version.

  | GET_BINDING_MODES
    -- ^ Get the names of all currently configured binding modes.

  | GET_CONFIG
    -- ^ Return the last loaded i3 config.

  | SEND_TICK
    -- ^ Send a tick event with the specified payload.

  | SYNC
    -- ^ Send an i3 sync event with the specified random value to the specified window.

  deriving ( Enum, Eq, Ord, Read, Show )


-- -----------------------------------------------------------------------------


-- | Send a message to i3.
sendMessage
 :: Connection
 -> MessageType -- ^ Type of message to send
 -> ByteString  -- ^ Payload to send
 -> IO ()

sendMessage (Connection sock) mType msg
  = sendAll sock $ runPut $ do
    putByteString "i3-ipc"
    putWord32host $ fromIntegral $ W.length msg
    putWord32host $ fromIntegral $ fromEnum mType
    putLazyByteString msg


-- | Receive a message from i3.
recvMessage
 :: Connection
 -> IO (Word32, ByteString) -- ^ Reply message type and payload

recvMessage (Connection sock)
  = do
    header <- recv sock 14
    if W.length header /= 14
      then fail $ printf "unexpected EOF while reading reply header,\
        \ expected 14 bytes but got %d" (W.length header)
      else do
        let (magic, mLen, mType) = runGet getHeader header
        if magic /= "i3-ipc"
          then fail $ printf "invalid magic, expected 'i3-ipc' but got '%s'"
            (C.unpack magic)
          else do
            payload <- recv sock $ fromIntegral mLen
            if W.length payload /= fromIntegral mLen
              then fail $ printf "unexpected EOF while reading reply payload,\
                \ expected %d bytes but got %d" mLen (W.length payload)
              else return ( mType, payload )

  where

    getHeader
      = (,,) <$> getLazyByteString 6 <*> getWord32host <*> getWord32host


-- | Send a message to i3, receive the reply and decode it.
exchange
 :: FromJSON a
 => Connection
 -> MessageType -- ^ Type of message to send
 -> ByteString  -- ^ Payload to send
 -> IO a        -- ^ Decoded reply

exchange con mType msg
  = do
    sendMessage con mType msg
    ( rTypeNum, payload ) <- recvMessage con
    if rTypeNum > 11
      then fail $ "invalid message type, expected integer in the range 0 to 11\
        \ but got " ++ show rTypeNum
      else do
        let rType = toEnum $ fromIntegral rTypeNum
        if rType /= mType
          then fail $ printf "unexpected message type, expected %s but got %s"
            (toReplyType mType) (toReplyType rType)
          else either fail return $ eitherDecode payload

  where

    toReplyType mType =
      [ "COMMAND", "WORKSPACES", "SUBSCRIBE", "OUTPUTS", "TREE", "MARKS"
      , "BAR_CONFIG", "VERSION", "BINDING_MODES", "CONFIG", "TICK", "SYNC"
      ] !! fromEnum mType :: String




-- -----------------------------------------------------------------------------
--
-- * Options for Aeson
--
-- -----------------------------------------------------------------------------


encodingOptions :: Options
encodingOptions
  = defaultOptions
  { fieldLabelModifier     = camelTo2 '_' . drop 3
  , constructorTagModifier = camelTo2 '_' . drop 3
  , sumEncoding            = UntaggedValue
  , unwrapUnaryRecords     = True
  }


keyOptions :: JSONKeyOptions
keyOptions
  = defaultJSONKeyOptions
  { keyModifier = camelTo2 '_' . drop 3
  }


encodePretty :: ToJSON a => a -> ByteString
encodePretty
  = encodePretty' defConfig
  { confIndent          = Spaces 2
  , confCompare         = compare
  , confTrailingNewline = True
  }
