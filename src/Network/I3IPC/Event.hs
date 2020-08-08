
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.I3IPC.Event where

import Data.Aeson
import Data.Text              ( Text )
import GHC.Generics

import Data.ByteString.Lazy.Char8 as C

import Network.I3IPC.Internal
import Network.I3IPC.Reply




-- -----------------------------------------------------------------------------
--
-- * Event
--
-- -----------------------------------------------------------------------------


-- | Represents an i3 event.
data Event

  = WorkspaceEvent              !WorkspaceEventInfo
    -- ^ Sent when the user switches to a different workspace, when a new
    -- workspace is initialized or when a workspace is removed (because the last
    -- client vanished).

  | OutputEvent                 !OutputEventInfo
    -- ^ Sent when RandR issues a change notification (of either screens,
    -- outputs, CRTCs or output properties).

  | ModeEvent                   !ModeEventInfo
    -- ^ Sent whenever i3 changes its binding mode.

  | WindowEvent                 !WindowEventInfo
    -- ^ Sent when a client's window is successfully reparented (that is when i3
    -- has finished fitting it into a container), when a window received input
    -- focus or when certain properties of the window have changed.

  | BarconfigUpdateEvent        !BarconfigEventInfo
    -- ^ Sent when the @hidden_state@ or @mode@ field in the barconfig of any
    -- bar instance was updated and when the config is reloaded.

  | BindingEvent                !BindingEventInfo
    -- ^ Sent when a configured command binding is triggered with the keyboard
    -- or mouse.

  | ShutdownEvent               !ShutdownEventInfo
    -- ^ Sent when the IPC shuts down because of a restart or exit by user
    -- command.

  | TickEvent                   !TickEventInfo
    -- ^ Sent when the IPC client subscribes to the tick event
    -- (with @"first": true@) or when any IPC client sends a `SEND_TICK` message
    -- (with @"first": false@).

  deriving ( Eq, Show )




-- -----------------------------------------------------------------------------
--
-- * Workspace event
--
-- -----------------------------------------------------------------------------


data WorkspaceEventInfo

  = WorkspaceEventInfo

  { we'change                :: !WorkspaceChange
    -- ^ Indicates the type of the change.

  , we'workspace             :: !(Maybe Node)
    -- ^ Will be present with the affected workspace whenever the type of event
    -- affects a workspace.

  , we'old                   :: !(Maybe Node)
    -- ^ Will be present with an old workspace whenever the change is @focus@.

  }

  deriving ( Eq, Generic )


instance FromJSON WorkspaceEventInfo
  where
    parseJSON
      = genericParseJSON encodingOptions


instance ToJSON WorkspaceEventInfo
  where
    toJSON
      = genericToJSON encodingOptions


instance Show WorkspaceEventInfo
  where
    show
      = C.unpack . encodePretty


-- -----------------------------------------------------------------------------


data WorkspaceChange

  = WC'Focus

  | WC'Init

  | WC'Empty

  | WC'Urgent

  | WC'Reload

  | WC'Rename

  | WC'Restored

  | WC'Move

  | WC'Unknown

  deriving ( Enum, Eq, Generic, Ord )


instance FromJSON WorkspaceChange
  where
    parseJSON
      = withText "WorkspaceChange" $ return <$> \ case
        "focus"    -> WC'Focus
        "init"     -> WC'Init
        "empty"    -> WC'Empty
        "urgent"   -> WC'Urgent
        "reload"   -> WC'Reload
        "rename"   -> WC'Rename
        "restored" -> WC'Restored
        "move"     -> WC'Move
        _          -> WC'Unknown


instance ToJSON WorkspaceChange
  where
    toJSON
      = genericToJSON encodingOptions


instance Show WorkspaceChange
  where
    show
      = C.unpack . encode




-- -----------------------------------------------------------------------------
--
-- * Output event
--
-- -----------------------------------------------------------------------------


data OutputEventInfo

  = OutputEventInfo

  { oe'change                :: !OutputChange
    -- ^ Indicates the type of the change (currently only @unspecified@).
  }

  deriving ( Eq, Generic )


instance FromJSON OutputEventInfo
  where
    parseJSON
      = genericParseJSON encodingOptions { unwrapUnaryRecords = False }


instance ToJSON OutputEventInfo
  where
    toJSON
      = genericToJSON encodingOptions { unwrapUnaryRecords = False }


instance Show OutputEventInfo
  where
    show
      = C.unpack . encodePretty


-- -----------------------------------------------------------------------------


data OutputChange

  = OC'Unspecified

  deriving ( Enum, Eq, Generic, Ord )


instance FromJSON OutputChange
  where
    parseJSON
      = genericParseJSON encodingOptions { tagSingleConstructors = True }


instance ToJSON OutputChange
  where
    toJSON
      = genericToJSON encodingOptions { tagSingleConstructors = True }


instance Show OutputChange
  where
    show
      = C.unpack . encode




-- -----------------------------------------------------------------------------
--
-- * Mode event
--
-- -----------------------------------------------------------------------------


data ModeEventInfo

  = ModeEventInfo

  { me'change                :: !Text
    -- ^ The name of current mode in use.

  , me'pangoMarkup           :: !Bool
    -- ^ Defines whether pango markup shall be used for displaying this mode.

  }

  deriving ( Eq, Generic )


instance FromJSON ModeEventInfo
  where
    parseJSON
      = genericParseJSON encodingOptions


instance ToJSON ModeEventInfo
  where
    toJSON
      = genericToJSON encodingOptions


instance Show ModeEventInfo
  where
    show
      = C.unpack . encodePretty




-- -----------------------------------------------------------------------------
--
-- * Window event
--
-- -----------------------------------------------------------------------------


data WindowEventInfo

  = WindowEventInfo

  { ie'change                :: !WindowChange
    -- ^ Indicates the type of the change.

  , ie'container             :: !Node
    -- ^ Consists of the window's parent container. Be aware that for the @new@
    -- event, the container will hold the initial name of the newly reparented
    -- window (e.g. if you run urxvt with a shell that changes the title, you
    -- will still at this point get the window title as @urxvt@).

  }

  deriving ( Eq, Generic )


instance FromJSON WindowEventInfo
  where
    parseJSON
      = genericParseJSON encodingOptions


instance ToJSON WindowEventInfo
  where
    toJSON
      = genericToJSON encodingOptions


instance Show WindowEventInfo
  where
    show
      = C.unpack . encodePretty


-- -----------------------------------------------------------------------------


data WindowChange

  = IC'New
    -- ^ The window has become managed by i3.

  | IC'Close
    -- ^ The window has closed.

  | IC'Focus
    -- ^ The window has received input focus.

  | IC'Title
    -- ^ The window's title has changed.

  | IC'FullscreenMode
    -- ^ The window has entered or exited fullscreen mode.

  | IC'Move
    -- ^ The window has changed its position in the tree.

  | IC'Floating
    -- ^ The window has transitioned to or from floating.

  | IC'Urgent
    -- ^ The window has become urgent or lost its urgent status.

  | IC'Mark
    -- ^ A mark has been added to or removed from the window.

  | IC'Unknown

  deriving ( Enum, Eq, Generic, Ord )


instance FromJSON WindowChange
  where
    parseJSON
      = withText "WindowChange" $ return <$> \ case
        "new"             -> IC'New
        "close"           -> IC'Close
        "focus"           -> IC'Focus
        "title"           -> IC'Title
        "fullscreen_mode" -> IC'FullscreenMode
        "move"            -> IC'Move
        "floating"        -> IC'Floating
        "urgent"          -> IC'Urgent
        "mark"            -> IC'Mark
        _                 -> IC'Unknown


instance ToJSON WindowChange
  where
    toJSON
      = genericToJSON encodingOptions


instance Show WindowChange
  where
    show
      = C.unpack . encode




-- -----------------------------------------------------------------------------
--
-- * Barconfig event
--
-- -----------------------------------------------------------------------------


data BarconfigEventInfo

  = BarconfigEventInfo

  { ce'barconfig             :: !BarConfig
    -- ^ Options from the barconfig of the specified bar_id that were updated in
    -- i3. This event is the same as a `GET_BAR_CONFIG` reply for the bar with
    -- the given id.
  }

  deriving ( Eq, Generic )


instance FromJSON BarconfigEventInfo
  where
    parseJSON
      = genericParseJSON encodingOptions


instance ToJSON BarconfigEventInfo
  where
    toJSON
      = genericToJSON encodingOptions


instance Show BarconfigEventInfo
  where
    show
      = C.unpack . encodePretty




-- -----------------------------------------------------------------------------
--
-- * Binding event
--
-- -----------------------------------------------------------------------------


data BindingEventInfo

  = BindingEventInfo

  { be'change                :: !BindingChange
    -- ^ Indicates what sort of binding event was triggered (right now it will
    -- always be @run@ but may be expanded in the future).

  , be'binding               :: !Binding
    -- ^ Contains details about the binding that was run.

  }

  deriving ( Eq, Generic )


instance FromJSON BindingEventInfo
  where
    parseJSON
      = genericParseJSON encodingOptions


instance ToJSON BindingEventInfo
  where
    toJSON
      = genericToJSON encodingOptions


instance Show BindingEventInfo
  where
    show
      = C.unpack . encodePretty


-- -----------------------------------------------------------------------------


data BindingChange

  = NC'Run

  deriving ( Enum, Eq, Generic, Ord )


instance FromJSON BindingChange
  where
    parseJSON
      = genericParseJSON encodingOptions { tagSingleConstructors = True }


instance ToJSON BindingChange
  where
    toJSON
      = genericToJSON encodingOptions { tagSingleConstructors = True }


instance Show BindingChange
  where
    show
      = C.unpack . encode


-- -----------------------------------------------------------------------------


data Binding

  = Binding

  { bd'command               :: !Text
    -- ^ The i3 command that is configured to run for this binding.

  , bd'eventStateMask        :: ![Text]
    -- ^ The group and modifier keys that were configured with this binding.

  , bd'inputCode             :: !Int
    -- ^ If the binding was configured with @bindcode@, this will be the key
    -- code that was given for the binding. If the binding is a mouse binding,
    -- it will be the number of the mouse button that was pressed. Otherwise it
    -- will be 0.

  , bd'symbol                :: !(Maybe Char)
    -- ^ If this is a keyboard binding that was configured with @bindsym@, this
    -- field will contain the given symbol. Otherwise it will be `Nothing`.

  , bd'inputType             :: !InputType
    -- ^ This will be @keyboard@ or @mouse@ depending on whether or not this was
    -- a keyboard or a mouse binding.

  }

  deriving ( Eq, Generic )


instance FromJSON Binding
  where
    parseJSON
      = genericParseJSON encodingOptions


instance ToJSON Binding
  where
    toJSON
      = genericToJSON encodingOptions


instance Show Binding
  where
    show
      = C.unpack . encodePretty


-- -----------------------------------------------------------------------------


data InputType

  = IT'Keyboard

  | IT'Mouse

  deriving ( Enum, Eq, Generic, Ord )


instance FromJSON InputType
  where
    parseJSON
      = genericParseJSON encodingOptions


instance ToJSON InputType
  where
    toJSON
      = genericToJSON encodingOptions


instance Show InputType
  where
    show
      = C.unpack . encode




-- -----------------------------------------------------------------------------
--
-- * Shutdown event
--
-- -----------------------------------------------------------------------------


-- | This event is triggered when the connection to the IPC is about to shutdown
-- because of a user action such as a restart or exit command.
data ShutdownEventInfo

  = ShutdownEventInfo

  { de'change                :: !ShutdownChange
    -- ^ Indicates why the IPC is shutting down. It can be either @restart@ or
    -- @exit@.
  }

  deriving ( Eq, Generic )


instance FromJSON ShutdownEventInfo
  where
    parseJSON
      = genericParseJSON encodingOptions


instance ToJSON ShutdownEventInfo
  where
    toJSON
      = genericToJSON encodingOptions


instance Show ShutdownEventInfo
  where
    show
      = C.unpack . encodePretty


-- -----------------------------------------------------------------------------


data ShutdownChange

  = SC'Restart

  | SC'Exit

  deriving ( Enum, Eq, Generic, Ord )


instance FromJSON ShutdownChange
  where
    parseJSON
      = genericParseJSON encodingOptions


instance ToJSON ShutdownChange
  where
    toJSON
      = genericToJSON encodingOptions


instance Show ShutdownChange
  where
    show
      = C.unpack . encode




-- -----------------------------------------------------------------------------
--
-- * Tick event
--
-- -----------------------------------------------------------------------------


-- | This event is triggered by a subscription to tick events or by a
-- `SEND_TICK` message.
data TickEventInfo

  = TickEventInfo

  { te'first                 :: !Bool
    -- ^ `True` when the IPC client subscribes to the tick event, `False` when
    -- any IPC client sends a `SEND_TICK` message.

  , te'payload               :: !Text
    -- ^ Payload of the `SEND_TICK` message.

  }

  deriving ( Eq, Generic )


instance FromJSON TickEventInfo
  where
    parseJSON
      = genericParseJSON encodingOptions


instance ToJSON TickEventInfo
  where
    toJSON
      = genericToJSON encodingOptions


instance Show TickEventInfo
  where
    show
      = C.unpack . encodePretty
