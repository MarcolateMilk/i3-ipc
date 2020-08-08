
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.I3IPC

  ( -- * Connection

    Connection
  , connect



    -- * Messaging

  , runCommand
  , getWorkspaces
  , getOutputs
  , getTree
  , getBarIds
  , getBarConfig
  , getVersion
  , getBindingModes
  , getConfig
  , sendTick



    -- * Event handling

  , EventType(..)
  , listen



    -- * Replies

    -- ** COMMAND reply
  , Commands(..)
  , Command(..)

    -- ** WORKSPACES reply
  , Workspaces(..)
  , Workspace(..)

    -- ** OUTPUTS reply
  , Outputs(..)
  , Output(..)

    -- ** TREE reply
  , Node(..)
  , NodeType(..)
  , NodeBorder(..)
  , NodeLayout(..)
  , WindowProperty(..)
  , WindowType(..)
  , FullscreenMode(..)

    -- ** MARKS reply
  , Marks(..)

    -- ** BAR_CONFIG reply
  , BarIds(..)
  , BarConfig(..)
  , BarMode(..)
  , BarPosition(..)
  , BarColor(..)

    -- ** VERSION reply
  , Version(..)

    -- ** BINDING_MODES reply
  , BindingModes(..)

    -- ** CONFIG reply
  , Config(..)

    -- ** TICK reply
  , Tick(..)

    -- ** SYNC reply
  , Sync(..)

    -- ** Common
  , Rect(..)



    -- * Events

    -- ** Event
  , Event(..)

    -- ** Workspace event
  , WorkspaceEventInfo(..)
  , WorkspaceChange(..)

    -- ** Output event
  , OutputEventInfo(..)
  , OutputChange(..)

    -- ** Mode event
  , ModeEventInfo(..)

    -- ** Window event
  , WindowEventInfo(..)
  , WindowChange(..)

    -- ** BarconfigUpdate event
  , BarconfigEventInfo(..)

    -- ** Binding event
  , BindingEventInfo(..)
  , BindingChange(..)
  , Binding(..)
  , InputType(..)

    -- ** Shutdown event
  , ShutdownEventInfo(..)
  , ShutdownChange(..)

    -- ** Tick event
  , TickEventInfo(..)

  )

  where


import Data.Aeson
import Data.Bits
import Data.ByteString.Lazy   ( ByteString )
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as C

import Network.I3IPC.Event
import Network.I3IPC.Internal
import Network.I3IPC.Reply




-- -----------------------------------------------------------------------------
--
-- * Messaging
--
-- -----------------------------------------------------------------------------


-- | Run the payload as an i3 command (like the commands you can bind to keys).
runCommand :: Connection -> ByteString -> IO Commands
runCommand con cmd
  = exchange con RUN_COMMAND cmd


-- | Get the list of current workspaces.
getWorkspaces :: Connection -> IO Workspaces
getWorkspaces con
  = exchange con GET_WORKSPACES ""


-- | Get the list of current outputs.
getOutputs :: Connection -> IO Outputs
getOutputs con
  = exchange con GET_OUTPUTS ""


-- | Get the i3 layout tree.
getTree :: Connection -> IO Node
getTree con
  = exchange con GET_TREE ""


-- | Get the names of all bar configurations.
getBarIds :: Connection -> IO BarIds
getBarIds con
  = exchange con GET_BAR_CONFIG ""


-- | Get the specified bar configuration.
getBarConfig :: Connection -> ByteString -> IO BarConfig
getBarConfig con barId
  = exchange con GET_BAR_CONFIG barId


-- | Get the i3 version.
getVersion :: Connection -> IO Version
getVersion con
  = exchange con GET_VERSION ""


-- | Get the names of all currently configured binding modes.
getBindingModes :: Connection -> IO BindingModes
getBindingModes con
  = exchange con GET_BINDING_MODES ""


-- | Get the last loaded i3 config.
getConfig :: Connection -> IO Config
getConfig con
  = exchange con GET_BINDING_MODES ""


-- | Send a tick event with the specified payload.
sendTick :: Connection -> ByteString -> IO Tick
sendTick con msg
  = exchange con SEND_TICK msg




-- -----------------------------------------------------------------------------
--
-- * Event handling
--
-- -----------------------------------------------------------------------------


data EventType

  = ET'Workspace

  | ET'Output

  | ET'Mode

  | ET'Window

  | ET'BarconfigUpdate

  | ET'Binding

  | ET'Shutdown

  | ET'Tick

  deriving ( Enum, Eq, Generic, Ord )


instance FromJSON EventType
  where
    parseJSON
      = genericParseJSON encodingOptions


instance ToJSON EventType
  where
    toJSON
      = genericToJSON encodingOptions


instance Show EventType
  where
    show
      = C.unpack . encode


-- -----------------------------------------------------------------------------


-- | Listen for events indefinately.
--
-- This function creates a seperate connection for event handling only to avoid
-- race conditions.
listen
  :: [EventType]      -- ^ Events to listen to
  -> (Event -> IO ()) -- ^ Event handler
  -> IO ()

listen types handler
  = do
    con <- connect
    sub <- exchange con SUBSCRIBE (encode types)
    if sb'success sub
      then loop con
      else fail "failed to subscribe to events"

  where

    loop con
      = do
        ( eType, payload ) <- recvEvent con
        event <- case eType of
          ET'Workspace       -> WorkspaceEvent       <$> buildInfo payload
          ET'Output          -> OutputEvent          <$> buildInfo payload
          ET'Mode            -> ModeEvent            <$> buildInfo payload
          ET'Window          -> WindowEvent          <$> buildInfo payload
          ET'BarconfigUpdate -> BarconfigUpdateEvent <$> buildInfo payload
          ET'Binding         -> BindingEvent         <$> buildInfo payload
          ET'Shutdown        -> ShutdownEvent        <$> buildInfo payload
          ET'Tick            -> TickEvent            <$> buildInfo payload
        handler event
        loop con

    recvEvent con
      = do
        ( eTypeNum, payload ) <- recvMessage con
        if not (eTypeNum `testBit` 31)
          then fail $ "invalid message type, expected event but got "
            ++ show eTypeNum
          else do
            let eTypeNum' = eTypeNum `clearBit` 31
            if eTypeNum' > 7
              then fail $ "invalid event type, expected integer in the range\
                \ 0 to 7 but got " ++ show eTypeNum'
              else return ( toEnum $ fromIntegral eTypeNum', payload )

    buildInfo payload
      = either fail return $ eitherDecode payload
