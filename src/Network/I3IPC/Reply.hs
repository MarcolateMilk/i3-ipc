
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.I3IPC.Reply where

import Data.Aeson
import Data.Aeson.Encoding
import Data.Aeson.Encode.Pretty ( encodePretty )
import Data.Map                 ( Map )
import Data.Scientific          ( floatingOrInteger )
import Data.Text                ( Text )
import GHC.Generics

import Network.I3IPC.Internal

import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy.Char8 as C




-- -----------------------------------------------------------------------------
--
-- * COMMAND reply
--
-- -----------------------------------------------------------------------------


-- | Reply to the `RUN_COMMAND` message.
data Commands

  = Commands

  { cm'results               :: ![Command]

  }

  deriving ( Eq, Generic )


instance FromJSON Commands
  where
    parseJSON
      = genericParseJSON encodingOptions


instance ToJSON Commands
  where
    toJSON
      = genericToJSON encodingOptions


instance Show Commands
  where
    show
      = C.unpack . encodePretty


-- -----------------------------------------------------------------------------


-- | Result of a single command.
data Command

  = Command

  { cm'success               :: !Bool
    -- ^ Whether the command was successful.

  , cm'error                 :: !(Maybe Text)
    -- ^ A human-readable error message.

  }

  deriving ( Eq, Generic )


instance FromJSON Command
  where
    parseJSON
      = genericParseJSON encodingOptions


instance ToJSON Command
  where
    toJSON
      = genericToJSON encodingOptions


instance Show Command
  where
    show
      = C.unpack . encodePretty




-- -----------------------------------------------------------------------------
--
-- * WORKSPACES reply
--
-- -----------------------------------------------------------------------------


-- | Reply to the `GET_WORKSPACES` message.
data Workspaces

  = Workspaces

  { ws'workspaces            :: ![Workspace]

  }

  deriving ( Eq, Generic )


instance FromJSON Workspaces
  where
    parseJSON
      = genericParseJSON encodingOptions


instance ToJSON Workspaces
  where
    toJSON
      = genericToJSON encodingOptions


instance Show Workspaces
  where
    show
      = C.unpack . encodePretty


-- -----------------------------------------------------------------------------


-- | A single workspace.
data Workspace

  = Workspace

  { ws'num                   :: !Int
    -- ^ The logical number of the workspace. Corresponds to the command to
    -- switch to this workspace. For named workspaces, this will be -1.

  , ws'name                  :: !Text
    -- ^ The name of this workspace (by default num+1), as changed by the user.

  , ws'visible               :: !Bool
    -- ^ Whether this workspace is currently visible on an output (multiple
    -- workspaces can be visible at the same time).

  , ws'focused               :: !Bool
    -- ^ Whether this workspace currently has the focus (only one workspace can
    -- have the focus at the same time).

  , ws'urgent                :: !Bool
    -- ^ Whether a window on this workspace has the urgent flag set.

  , ws'rect                  :: !Rect
    -- ^ The rectangle of this workspace (equals the rect of the output it is on).

  , ws'output                :: !Text
    -- ^ The video output this workspace is on (LVDS1, VGA1, …).

  }

  deriving ( Eq, Generic )


instance FromJSON Workspace
  where
    parseJSON
      = genericParseJSON encodingOptions


instance ToJSON Workspace
  where
    toJSON
      = genericToJSON encodingOptions


instance Show Workspace
  where
    show
      = C.unpack . encodePretty




-- -----------------------------------------------------------------------------
--
-- * SUBSCRIBE reply
--
-- -----------------------------------------------------------------------------


-- | Reply to the `SUBSCRIBE` message.
data Subscribe

  = Subscribe

  { sb'success               :: !Bool
    -- ^ Whether the subscription was successful (the default) or whether a
    -- JSON parse error occurred.

  }

  deriving ( Eq, Generic )


instance FromJSON Subscribe
  where
    parseJSON
      = genericParseJSON encodingOptions { unwrapUnaryRecords = False }


instance ToJSON Subscribe
  where
    toJSON
      = genericToJSON encodingOptions { unwrapUnaryRecords = False }


instance Show Subscribe
  where
    show
      = C.unpack . encodePretty




-- -----------------------------------------------------------------------------
--
-- * OUTPUTS reply
--
-- -----------------------------------------------------------------------------


-- | Reply to the `GET_OUTPUTS` message.
data Outputs

  = Outputs

  { op'outputs               :: ![Output]

  }

  deriving ( Eq, Generic )


instance FromJSON Outputs
  where
    parseJSON
      = genericParseJSON encodingOptions


instance ToJSON Outputs
  where
    toJSON
      = genericToJSON encodingOptions


instance Show Outputs
  where
    show
      = C.unpack . encodePretty


-- -----------------------------------------------------------------------------


-- | A single output.
data Output

  = Output

  { op'name                  :: !Text
    -- ^ The name of this output (as seen in xrandr(1)).

  , op'active                :: !Bool
    -- ^ Whether this output is currently active (has a valid mode).

  , op'primary               :: !Bool
    -- ^ Whether this output is currently the primary output.

  , op'currentWorkspace      :: !(Maybe Text)
    -- ^ The name of the current workspace that is visible on this output.
    -- `Nothing` if the output is not active.

  , op'rect                  :: !Rect
    -- ^ The rectangle of this output (equals the rect of the output it is on).

  }

  deriving ( Eq, Generic )


instance FromJSON Output
  where
    parseJSON
      = genericParseJSON encodingOptions


instance ToJSON Output
  where
    toJSON
      = genericToJSON encodingOptions


instance Show Output
  where
    show
      = C.unpack . encodePretty




-- -----------------------------------------------------------------------------
--
-- * TREE reply
--
-- -----------------------------------------------------------------------------


-- | Reply to the `GET_TREE` message.
data Node

  = Node

  { nd'id                    :: !Int
    -- ^ The internal ID (actually a C pointer value) of this container.
    -- Do not make any assumptions about it. You can use it to (re-)identify
    -- and address containers when talking to i3.

  , nd'name                  :: !(Maybe Text)
    -- ^ The internal name of this container. For all containers which are part
    -- of the tree structure down to the workspace contents, this is set to a
    -- nice human-readable name of the container. For containers that have an
    -- X11 window, the content is the title (_NET_WM_NAME property) of that
    -- window. For all other containers, the content is not defined (yet).

  , nd'type                  :: !NodeType
    -- ^ Type of this container. Can be one of @root@, @output@, @con@,
    -- @floating_con@, @workspace@ or @dockarea@.

  , nd'border                :: !NodeBorder
    -- ^ Can be either @normal@, @none@ or @pixel@, depending on the container’s
    -- border style.

  , nd'currentBorderWidth    :: !Int
    -- ^ Number of pixels of the border width.

  , nd'layout                :: !NodeLayout
    -- ^ Can be either @splith@, @splitv@, @stacked@, @tabbed@, @dockarea@ or
    -- @output@. Other values might be possible in the future, should new
    -- layouts be added.

  , nd'percent               :: !(Maybe Double)
    -- ^ The percentage which this container takes in its parent. A value of
    -- `Nothing` means that the percent property does not make sense for this
    -- container, for example for the root container.

  , nd'rect                  :: !Rect
    -- ^ The absolute display coordinates for this container. Display coordinates
    -- means that when you have two 1600x1200 monitors on a single X11 Display
    -- (the standard way), the coordinates of the first window on the second
    -- monitor are @{ "x": 1600, "y": 0, "width": 1600, "height": 1200 }@.

  , nd'windowRect            :: !Rect
    -- ^ The coordinates of the actual client window inside its container. These
    -- coordinates are relative to the container and do not include the window
    -- decoration (which is actually rendered on the parent container). So,
    -- when using the default layout, you will have a 2 pixel border on each
    -- side, making the window-rect
    -- @{ "x": 2, "y": 0, "width": 632, "height": 366 }@ (for example).

  , nd'decoRect              :: !Rect
    -- ^ The coordinates of the window decoration inside its container. These
    -- coordinates are relative to the container and do not include the actual
    -- client window.

  , nd'geometry              :: !Rect
    -- ^ The original geometry the window specified when i3 mapped it. Used when
    -- switching a window to floating mode, for example.

  , nd'window                :: !(Maybe Int)
    -- ^ The X11 window ID of the actual client window inside this container.
    -- This field is set to `Nothing` for split containers or otherwise empty
    -- containers. This ID corresponds to what xwininfo(1) and other
    -- X11-related tools display (usually in hex).

  , nd'windowProperties      :: !(Maybe (Map WindowProperty (Maybe Text)))
    -- ^ This optional field contains all available X11 window properties.

  , nd'windowType            :: !(Maybe WindowType)
    -- ^ The window type (_NET_WM_WINDOW_TYPE). Possible values are @undefined@,
    -- @normal@, @dialog@, @utility@, @toolbar@, @splash@, @menu@,
    -- @dropdown_menu@, @popup_menu@, @tooltip@ and @notification@.

  , nd'urgent                :: !Bool
    -- ^ Whether this container (window, split container, floating container or
    -- workspace) has the urgency hint set, directly or indirectly. All parent
    -- containers up until the workspace container will be marked urgent if
    -- they have at least one urgent child.

  , nd'marks                 :: !(Maybe [Text])
    -- ^ List of marks assigned to container.

  , nd'focused               :: !Bool
    -- ^ Whether this container is currently focused.

  , nd'focus                 :: ![Int]
    -- ^ List of child node IDs (see nodes, floating nodes and id) in focus
    -- order. Traversing the tree by following the first entry in this list
    -- will result in eventually reaching the one node with focused set to true.

  , nd'fullscreenMode        :: !FullscreenMode
    -- ^ Whether this container is in fullscreen state or not. Note that all
    -- workspaces are considered fullscreened on their respective output.

  , nd'nodes                 :: ![Node]
    -- ^ The tiling (i.e. non-floating) child containers of this node.

  , nd'floatingNodes         :: ![Node]
    -- ^ The floating child containers of this node. Only non-empty on nodes
    -- with type workspace.

  }

  deriving ( Eq, Generic )


instance FromJSON Node
  where
    parseJSON
      = genericParseJSON encodingOptions


instance ToJSON Node
  where
    toJSON
      = genericToJSON encodingOptions


instance Show Node
  where
    show
      = C.unpack . encodePretty


-- -----------------------------------------------------------------------------


data NodeType

  = NT'Root

  | NT'Output

  | NT'Con

  | NT'FloatingCon

  | NT'Workspace

  | NT'Dockarea

  | NT'Unknown

  deriving ( Enum, Eq, Generic, Ord )


instance FromJSON NodeType
  where
    parseJSON
      = withText "NodeType" $ return <$> \ case
        "root"         -> NT'Root
        "output"       -> NT'Output
        "con"          -> NT'Con
        "floating_con" -> NT'FloatingCon
        "workspace"    -> NT'Workspace
        "dockarea"     -> NT'Dockarea
        _              -> NT'Unknown


instance ToJSON NodeType
  where
    toJSON
      = genericToJSON encodingOptions


instance Show NodeType
  where
    show
      = C.unpack . encode


-- -----------------------------------------------------------------------------


data NodeBorder

  = NB'Normal

  | NB'None

  | NB'Pixel

  | NB'Unknown

  deriving ( Enum, Eq, Generic, Ord )


instance FromJSON NodeBorder
  where
    parseJSON
      = withText "NodeBorder" $ return <$> \ case
        "normal" -> NB'Normal
        "none"   -> NB'Normal
        "pixel"  -> NB'Pixel
        _        -> NB'Unknown


instance ToJSON NodeBorder
  where
    toJSON
      = genericToJSON encodingOptions


instance Show NodeBorder
  where
    show
      = C.unpack . encode


-- -----------------------------------------------------------------------------


data NodeLayout

  = NL'Splith

  | NL'Splitv

  | NL'Stacked

  | NL'Tabbed

  | NL'Dockarea

  | NL'Output

  | NL'Unknown

  deriving ( Enum, Eq, Generic, Ord )


instance FromJSON NodeLayout
  where
    parseJSON
      = withText "NodeLayout" $ return <$> \ case
        "splith"   -> NL'Splith
        "splitv"   -> NL'Splitv
        "stacked"  -> NL'Stacked
        "tabbed"   -> NL'Tabbed
        "dockarea" -> NL'Dockarea
        "output"   -> NL'Output
        _          -> NL'Unknown


instance ToJSON NodeLayout
  where
    toJSON
      = genericToJSON encodingOptions


instance Show NodeLayout
  where
    show
      = C.unpack . encode


-- -----------------------------------------------------------------------------


data WindowProperty

  = WP'Title

  | WP'Instance

  | WP'Class

  | WP'WindowRole

  | WP'TransientFor

  deriving ( Enum, Eq, Generic, Ord )


instance FromJSON WindowProperty
  where
    parseJSON
      = genericParseJSON encodingOptions


instance ToJSON WindowProperty
  where
    toJSON
      = genericToJSON encodingOptions


instance FromJSONKey WindowProperty
  where
    fromJSONKey
      = genericFromJSONKey keyOptions


instance ToJSONKey WindowProperty
  where
    toJSONKey
      = genericToJSONKey keyOptions


instance Show WindowProperty
  where
    show
      = C.unpack . encode


-- -----------------------------------------------------------------------------


data WindowType

  = WT'Normal

  | WT'Dialog

  | WT'Utility

  | WT'Toolbar

  | WT'Splash

  | WT'Menu

  | WT'DropdownMenu

  | WT'PopupMenu

  | WT'Tooltip

  | WT'Notification

  | WT'Unknown

  deriving ( Enum, Eq, Generic, Ord )


instance FromJSON WindowType
  where
    parseJSON
      = withText "WindowType" $ return <$> \ case
        "normal"        -> WT'Normal
        "dialog"        -> WT'Dialog
        "utility"       -> WT'Utility
        "toolbar"       -> WT'Toolbar
        "splash"        -> WT'Splash
        "menu"          -> WT'Menu
        "dropdown_menu" -> WT'DropdownMenu
        "popup_menu"    -> WT'PopupMenu
        "tooltip"       -> WT'Tooltip
        "notification"  -> WT'Notification
        _               -> WT'Unknown


instance ToJSON WindowType
  where
    toJSON
      = genericToJSON encodingOptions


instance Show WindowType
  where
    show
      = C.unpack . encode


-- -----------------------------------------------------------------------------


data FullscreenMode

  = FM'None

  | FM'Output

  | FM'Global

  deriving ( Enum, Eq, Generic, Ord )


instance FromJSON FullscreenMode
  where
    parseJSON
      = withScientific "FullscreenMode" $ \ v -> do
        case floatingOrInteger v of
          Left  _ -> fail "parsing FullscreenMode failed, expected integer but\
            \ got floating value"
          Right n -> if 0 > n || n > 2
            then fail $ "parsing FullscreenMode failed, expected integer in the\
              \ range 0 to 2 but got " ++ show n
            else return $ toEnum n


instance ToJSON FullscreenMode
  where
    toJSON
      = Number . fromIntegral . fromEnum

    toEncoding
      = text . T.pack . show . fromEnum


instance Show FullscreenMode
  where
    show
      = C.unpack . encode




-- -----------------------------------------------------------------------------
--
-- * MARKS reply
--
-- -----------------------------------------------------------------------------


-- | Reply to the `GET_MARKS` message.
--
-- Consists of a single list of Texts for each container that has a mark.
-- A mark can only be set on one container, so the list is unique. The order
-- of that list is undefined.
--
-- If no window has a mark the response will be the empty list @[]@.
data Marks

  = Marks

  { mr'marks                 :: ![Text]

  }

  deriving ( Eq, Generic )


instance FromJSON Marks
  where
    parseJSON
      = genericParseJSON encodingOptions


instance ToJSON Marks
  where
    toJSON
      = genericToJSON encodingOptions


instance Show Marks
  where
    show
      = C.unpack . encodePretty




-- -----------------------------------------------------------------------------
--
-- * BAR_CONFIG reply
--
-- -----------------------------------------------------------------------------


-- | Reply to the `GET_BAR_CONFIG` message with empty input.
--
-- Consists of a list of configured bar ids.
data BarIds

  = BarIds

  { bc'ids                   :: ![Text]

  }

  deriving ( Eq, Generic )


instance FromJSON BarIds
  where
    parseJSON
      = genericParseJSON encodingOptions


instance ToJSON BarIds
  where
    toJSON
      = genericToJSON encodingOptions


instance Show BarIds
  where
    show
      = C.unpack . encodePretty


-- -----------------------------------------------------------------------------


-- | Reply to the `GET_BAR_CONFIG` message.
--
-- This can be used by third-party workspace bars (especially i3bar, but others
-- are free to implement compatible alternatives) to get the bar block
-- configuration from i3.
data BarConfig

  = BarConfig

  { bc'id                    :: !Text
    -- ^ The ID for this bar. Included in case you request multiple
    -- configurations and want to differentiate the different replies.

  , bc'mode                  :: !BarMode
    -- ^ Either @dock@ (the bar sets the dock window type) or @hide@ (the bar
    -- does not show unless a specific key is pressed).

  , bc'position              :: !BarPosition
    -- ^ Either @bottom@ or @top@ at the moment.

  , bc'statusCommand         :: !Text
    -- ^ Command which will be run to generate a statusline. Each line on stdout
    -- of this command will be displayed in the bar. At the moment, no
    -- formatting is supported.

  , bc'font                  :: !Text
    -- ^ The font to use for text on the bar.

  , bc'workspaceButtons      :: !Bool
    -- ^ Display workspace buttons or not? Defaults to true.

  , bc'bindingModeIndicator  :: !Bool
    -- ^ Display the mode indicator or not? Defaults to true.

  , bc'verbose               :: !Bool
    -- ^ Should the bar enable verbose output for debugging? Defaults to false.

  , bc'colors                :: !(Map BarColor Text)
    -- ^ Contains a map of colors. Each value is a color code in hex, formatted
    -- #rrggbb (like in HTML).

  }

  deriving ( Eq, Generic )


instance FromJSON BarConfig
  where
    parseJSON
      = genericParseJSON encodingOptions


instance ToJSON BarConfig
  where
    toJSON
      = genericToJSON encodingOptions


instance Show BarConfig
  where
    show
      = C.unpack . encodePretty


-- -----------------------------------------------------------------------------


data BarMode

  = BM'Dock
    -- ^ The bar sets the dock window type.

  | BM'Hide
    -- ^ The bar does not show unless a specific key is pressed.

  | BM'Unknown

  deriving ( Enum, Eq, Generic, Ord )


instance FromJSON BarMode
  where
    parseJSON
      = withText "BarMode" $ return <$> \ case
        "dock" -> BM'Dock
        "hide" -> BM'Hide
        _      -> BM'Unknown


instance ToJSON BarMode
  where
    toJSON
      = genericToJSON encodingOptions


instance Show BarMode
  where
    show
      = C.unpack . encode


-- -----------------------------------------------------------------------------


data BarPosition

  = BP'Top

  | BP'Bottom

  | BP'Unknown

  deriving ( Enum, Eq, Generic, Ord )


instance FromJSON BarPosition
  where
    parseJSON
      = withText "BarPosition" $ return <$> \ case
        "top"    -> BP'Top
        "bottom" -> BP'Bottom
        _        -> BP'Unknown


instance ToJSON BarPosition
  where
    toJSON
      = genericToJSON encodingOptions


instance Show BarPosition
  where
    show
      = C.unpack . encode


-- -----------------------------------------------------------------------------


data BarColor

  = BC'Background
    -- ^ Background color of the bar.

  | BC'Statusline
    -- ^ Foreground color to be used for the statusline.

  | BC'Seperator
    -- ^ Foreground color to be used for the separator.

  | BC'FocusedBackground
    -- ^ Background color of the bar on the currently focused monitor output.

  | BC'FocusedStatusline
    -- ^ Foreground color to be used for the statusline on the currently focused
    -- monitor output.

  | BC'FocusedSeperator
    -- ^ Foreground color to be used for the separator on the currently focused
    -- monitor output.

  | BC'FocusedWorkspaceText
    -- ^ Foreground color for a workspace button when the workspace has focus.

  | BC'FocusedWorkspaceBg
    -- ^ Background color for a workspace button when the workspace has focus.

  | BC'FocusedWorkspaceBorder
    -- ^ Border color for a workspace button when the workspace has focus.

  | BC'ActiveWorkspaceText
    -- ^ Foreground color for a workspace button when the workspace is active
    -- (visible) on some output, but the focus is on another one. You can only
    -- tell this apart from the focused workspace when you are using multiple
    -- monitors.

  | BC'ActiveWorkspaceBg
    -- ^ Background color for a workspace button when the workspace is active
    -- (visible) on some output, but the focus is on another one. You can only
    -- tell this apart from the focused workspace when you are using multiple
    -- monitors.

  | BC'ActiveWorkspaceBorder
    -- ^ Border color for a workspace button when the workspace is active
    -- (visible) on some output, but the focus is on another one. You can only
    -- tell this apart from the focused workspace when you are using multiple
    -- monitors.

  | BC'InactiveWorkspaceText
    -- ^ Foreground color for a workspace button when the workspace does not have
    -- focus and is not active (visible) on any output. This will be the case
    -- for most workspaces.

  | BC'InactiveWorkspaceBg
    -- ^ Background color for a workspace button when the workspace does not have
    -- focus and is not active (visible) on any output. This will be the case
    -- for most workspaces.

  | BC'InactiveWorkspaceBorder
    -- ^ Border color for a workspace button when the workspace does not have
    -- focus and is not active (visible) on any output. This will be the case
    -- for most workspaces.

  | BC'UrgentWorkspaceText
    -- ^ Foreground color for workspaces which contain at least one window with
    -- the urgency hint set.

  | BC'UrgentWorkspaceBg
    -- ^ Background color for workspaces which contain at least one window with
    -- the urgency hint set.

  | BC'UrgentWorkspaceBorder
    -- ^ Border color for workspaces which contain at least one window with the
    -- urgency hint set.

  | BC'BindingModeText
    -- ^ Foreground color for the binding mode indicator.

  | BC'BindingModeBg
    -- ^ Background color for the binding mode indicator.

  | BC'BindingModeBorder
    -- ^ Border color for the binding mode indicator.

  deriving ( Enum, Eq, Generic, Ord )


instance FromJSON BarColor
  where
    parseJSON
      = genericParseJSON encodingOptions


instance ToJSON BarColor
  where
    toJSON
      = genericToJSON encodingOptions


instance FromJSONKey BarColor
  where
    fromJSONKey
      = genericFromJSONKey keyOptions


instance ToJSONKey BarColor
  where
    toJSONKey
      = genericToJSONKey keyOptions


instance Show BarColor
  where
    show
      = C.unpack . encode




-- -----------------------------------------------------------------------------
--
-- * VERSION reply
--
-- -----------------------------------------------------------------------------


-- | Reply to the `GET_VERSION` message.
data Version

  = Version

  { vr'major                 :: !Int
    -- ^ The major version of i3, such as 4.

  , vr'minor                 :: !Int
    -- ^ The minor version of i3, such as 2. Changes in the IPC interface (new
    -- features) will only occur with new minor (or major) releases. However,
    -- bugfixes might be introduced in patch releases, too.

  , vr'patch                 :: !Int
    -- ^ The patch version of i3, such as 1 (when the complete version is 4.2.1).
    -- For versions such as 4.2, patch will be set to 0.

  , vr'humanReadable         :: !Text
    -- ^ A human-readable version of i3 containing the precise git version,
    -- build date and branch name. When you need to display the i3 version to
    -- your users, use the human-readable version whenever possible (since
    -- this is what i3 --version displays, too).

  , vr'loadedConfigFileName  :: !Text
    -- ^ The current config path.

  }

  deriving ( Eq, Generic )


instance FromJSON Version
  where
    parseJSON
      = genericParseJSON encodingOptions


instance ToJSON Version
  where
    toJSON
      = genericToJSON encodingOptions


instance Show Version
  where
    show
      = C.unpack . encodePretty




-- -----------------------------------------------------------------------------
--
-- * BINDING_MODES reply
--
-- -----------------------------------------------------------------------------


-- | Reply to the `GET_BINDING_MODES` message.
--
-- Consists of a list of all currently configured binding modes.
data BindingModes

  = BindingModes

  { bm'modes                 :: ![Text]

  }

  deriving ( Eq, Generic )


instance FromJSON BindingModes
  where
    parseJSON
      = genericParseJSON encodingOptions


instance ToJSON BindingModes
  where
    toJSON
      = genericToJSON encodingOptions


instance Show BindingModes
  where
    show
      = C.unpack . encodePretty




-- -----------------------------------------------------------------------------
--
-- * CONFIG reply
--
-- -----------------------------------------------------------------------------


-- | Reply to the `GET_CONFIG` message.
--
-- Contains the config file as loaded by i3 most recently.
data Config

  = Config

  { cf'config                :: !Text

  }

  deriving ( Eq, Generic )


instance FromJSON Config
  where
    parseJSON
      = genericParseJSON encodingOptions


instance ToJSON Config
  where
    toJSON
      = genericToJSON encodingOptions


instance Show Config
  where
    show
      = C.unpack . encodePretty




-- -----------------------------------------------------------------------------
--
-- * TICK reply
--
-- -----------------------------------------------------------------------------


-- | Reply to the `SEND_TICK` message.
--
-- After the reply was received, the tick event has been written to all IPC
-- connections which subscribe to tick events. UNIX sockets are usually
-- buffered, but you can be certain that once you receive the tick event you
-- just triggered, you must have received all events generated prior to the
-- `SEND_TICK` message (happened-before relation).
data Tick

  = Tick

  { tk'success               :: !Bool

  }

  deriving ( Eq, Generic )


instance FromJSON Tick
  where
    parseJSON
      = genericParseJSON encodingOptions { unwrapUnaryRecords = False }


instance ToJSON Tick
  where
    toJSON
      = genericToJSON encodingOptions { unwrapUnaryRecords = False }


instance Show Tick
  where
    show
      = C.unpack . encodePretty




-- -----------------------------------------------------------------------------
--
-- * SYNC reply
--
-- -----------------------------------------------------------------------------


-- | Reply to the `SYNC` message.
--
-- After the reply was received, the i3 sync message was responded to.
data Sync

  = Sync

  { sc'success               :: !Bool

  }

  deriving ( Eq, Generic )


instance FromJSON Sync
  where
    parseJSON
      = genericParseJSON encodingOptions { unwrapUnaryRecords = False }


instance ToJSON Sync
  where
    toJSON
      = genericToJSON encodingOptions { unwrapUnaryRecords = False }


instance Show Sync
  where
    show
      = C.unpack . encodePretty




-- -----------------------------------------------------------------------------
--
-- * Common
--
-- -----------------------------------------------------------------------------


-- | Represents a rectangle with a position and size.
data Rect

  = Rect

  { rc'x                     :: !Int

  , rc'y                     :: !Int

  , rc'width                 :: !Int

  , rc'height                :: !Int

  }

  deriving ( Eq, Generic, Ord )


instance FromJSON Rect
  where
    parseJSON
      = genericParseJSON encodingOptions


instance ToJSON Rect
  where
    toJSON
      = genericToJSON encodingOptions


instance Show Rect
  where
    show
      = C.unpack . encodePretty
