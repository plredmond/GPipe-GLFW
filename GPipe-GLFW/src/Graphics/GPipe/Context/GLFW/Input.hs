-- | This module exposes much of the functionality that GLFW the __Input guide__ documents:
-- <http://www.glfw.org/docs/latest/input_guide.html>.
-- Actions are in the GPipe 'ContextT' monad when a window handle is required,
-- otherwise they are bare IO actions.
-- It is recommended to read about GLFW input handling as it pertains to your domain,
-- especially the __Event processing__ section:
-- <http://www.glfw.org/docs/latest/input_guide.html#events>.

module Graphics.GPipe.Context.GLFW.Input (

 -- * Event processing
 -- | Learn more: http://www.glfw.org/docs/latest/input_guide.html#events
 GLFW.pollEvents,
 -- | Process received events and return; for applications which continually render.
 GLFW.waitEvents,
 -- | Sleep until an event is received; for applications which update in response to user input.
 GLFW.postEmptyEvent,
 -- | Force wake from 'waitEvents' with a dummy event.

 -- * Keyboard input
 -- | Learn more: http://www.glfw.org/docs/latest/input_guide.html#input_keyboard

 -- ** Key input
 setKeyCallback,
 getKey,
 setStickyKeysInputMode,
 getStickyKeysInputMode,

 -- ** Text input
 setCharCallback,

 -- * Mouse input
 -- | Learn more: http://www.glfw.org/docs/latest/input_guide.html#input_mouse

 -- ** Cursor position
 setCursorPosCallback,
 getCursorPos,

 -- ** Cursor modes
 setCursorInputMode,
 getCursorInputMode,

 -- ** Cursor objects

 -- *** Custom cursor creation
 GLFW.createCursor,

 -- *** Standard cursor creation
 GLFW.createStandardCursor,

 -- *** Cursor destruction
 GLFW.destroyCursor,

 -- *** Cursor setting
 setCursor,

 -- ** Cursor enter/leave events
 setCursorEnterCallback,

 -- ** Mouse button input
 setMouseButtonCallback,
 getMouseButton,
 setStickyMouseButtonsInputMode,
 getStickyMouseButtonsInputMode,

 -- ** Scroll input
 setScrollCallback,

 -- * Joystick input
 -- | Learn more: http://www.glfw.org/docs/latest/input_guide.html#joystick
 GLFW.joystickPresent,
 -- | Is the specified 'Joystick' currently connected?

 -- ** Joystick axis states
 GLFW.getJoystickAxes,
 -- | Poll for the positions of each axis on the 'Joystick'. Positions are on the range `-1.0..1.0`.

 -- ** Joystick button states
 GLFW.getJoystickButtons,
 -- | Poll for the 'JoystickButtonState' of each button on the 'Joystick'.

 -- ** Joystick name
 GLFW.getJoystickName,
 -- | Retrieve a non-unique string identifier for the 'Joystick'.
 -- This might be the make & model name of the device.

 -- * Time input
 -- | Learn more: http://www.glfw.org/docs/latest/input_guide.html#time
 GLFW.getTime,
 -- | Poll for the number of seconds since GLFW was initialized by GPipe.
 GLFW.setTime,
 -- | Manually set the timer to a specified value.

 -- * Clipboard input and output
 -- | Learn more: http://www.glfw.org/docs/latest/input_guide.html#clipboard
 getClipboardString,
 setClipboardString,

 -- * Path drop input
 -- | Learn more: http://www.glfw.org/docs/latest/input_guide.html#path_drop
 setDropCallback,

 -- * Reexported datatypes

 -- ** Keyboard
 Key(..),
 KeyState(..),
 ModifierKeys(..),
 StickyKeysInputMode(..),

 -- ** Mouse
 CursorInputMode(..),
 StandardCursorShape(..),
 CursorState(..),
 StickyMouseButtonsInputMode(..),
 MouseButton(..),
 MouseButtonState(..),

 -- ** Joystick
 Joystick(..),
 JoystickButtonState(..),

 -- * Not supported
 -- | Some GLFW functionality isn't currently exposed by "GLFW-b".
 --
 --     * `glfwWaitEventsTimeout`
 --     * `glfwSetCharModsCallback`
 --     * `glfwGetKeyName`
 --     * `glfwSetJoystickCallback`
 --     * `glfwGetTimerValue`
 --     * `glfwGetTimerFrequency`
 ) where

-- stdlib
import Control.Monad.IO.Class (MonadIO)
import Graphics.GPipe.Context (ContextT, withContextWindow)

-- third party
import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFW (
 Key(..),
 KeyState(..),
 ModifierKeys(..),
 StickyKeysInputMode(..),
 CursorInputMode(..),
 Cursor(..),
 StandardCursorShape(..),
 CursorState(..),
 StickyMouseButtonsInputMode(..),
 MouseButton(..),
 MouseButtonState(..),
 Joystick(..),
 JoystickButtonState(..),
 )

-- local
import Graphics.GPipe.Context.GLFW.Resource (WrappedWindow(..))


{- Util -}

-- | Convenience function to access the unwrapped GLFW window.
withWindow :: MonadIO m => (GLFW.Window -> IO a) -> ContextT WrappedWindow os f m a
withWindow f = withContextWindow $ f . rawContext

-- | Convenience function to quickly wrap two argument functions taking window and something else.
-- Wrapped functions don't take window.
wrapWindowFun :: MonadIO m => (GLFW.Window -> a -> IO b) -> a -> ContextT WrappedWindow os f m b
wrapWindowFun f x = withWindow $ \w -> f w x

-- | Convenience function to quickly wrap callback setters taking window and passing window to the callback.
-- Wrapped functions don't take window.
-- Callbacks don't receive window /it is eaten by `const`/.
wrapCallbackSetter :: (MonadIO m, Functor g) => (GLFW.Window -> g (GLFW.Window -> b) -> IO a) -> g b -> ContextT WrappedWindow os f m a
wrapCallbackSetter setter cb = withWindow $ \w -> setter w (const <$> cb)

{- Keyboard -}

-- | Register or unregister a callback to receive 'KeyState' changes to any 'Key'.
setKeyCallback :: MonadIO m => Maybe (Key -> Int -> KeyState -> ModifierKeys -> IO ()) -> ContextT WrappedWindow os f m ()
setKeyCallback = wrapCallbackSetter GLFW.setKeyCallback

-- | Poll for the 'KeyState' of a 'Key'.
getKey :: MonadIO m => Key -> ContextT WrappedWindow os f m KeyState
getKey = wrapWindowFun GLFW.getKey

-- | Polling a 'Key' for 'KeyState' may sometimes miss state transitions.
-- If you use cannot use a callback to receive 'KeyState' changes,
-- use 'getKey' in combination with GLFW's sticky-keys feature:
-- <http://www.glfw.org/docs/latest/input_guide.html#input_key>.
setStickyKeysInputMode :: MonadIO m => StickyKeysInputMode -> ContextT WrappedWindow os f m ()
setStickyKeysInputMode = wrapWindowFun GLFW.setStickyKeysInputMode

getStickyKeysInputMode :: MonadIO m => ContextT WrappedWindow os f m StickyKeysInputMode
getStickyKeysInputMode = withWindow GLFW.getStickyKeysInputMode

-- | Register or unregister a callback to receive character input obeying keyboard layouts and modifier effects.
setCharCallback :: MonadIO m => Maybe (Char -> IO ()) -> ContextT WrappedWindow os f m ()
setCharCallback = wrapCallbackSetter GLFW.setCharCallback

{- Mouse -}

-- | Register or unregister a callback to receive mouse location changes.
-- Callback receives `x` and `y` position measured in screen-coordinates relative to the top left of the GLFW window.
setCursorPosCallback :: MonadIO m => Maybe (Double -> Double -> IO ()) -> ContextT WrappedWindow os f m ()
setCursorPosCallback = wrapCallbackSetter GLFW.setCursorPosCallback

-- | Poll for the location of the mouse.
getCursorPos :: MonadIO m => ContextT WrappedWindow os f m (Double, Double)
getCursorPos = withWindow GLFW.getCursorPos

-- | GLFW supports setting cursor mode to support mouselook and other advanced uses of the mouse:
-- <http://www.glfw.org/docs/latest/input_guide.html#cursor_mode>.
setCursorInputMode :: MonadIO m => CursorInputMode -> ContextT WrappedWindow os f m ()
setCursorInputMode = wrapWindowFun GLFW.setCursorInputMode

getCursorInputMode :: MonadIO m => ContextT WrappedWindow os f m CursorInputMode
getCursorInputMode = withWindow GLFW.getCursorInputMode

-- | Set the cursor to be displayed over the window while 'CursorInputMode' is `Normal`.
setCursor :: MonadIO m => Cursor -> ContextT WrappedWindow os f m ()
setCursor = wrapWindowFun GLFW.setCursor

-- | Register or unregister a callback to receive 'CursorState' changes when the cursor enters or exits the window.
setCursorEnterCallback :: MonadIO m => Maybe (CursorState -> IO ()) -> ContextT WrappedWindow os f m ()
setCursorEnterCallback = wrapCallbackSetter GLFW.setCursorEnterCallback

-- | Register or unregister a callback to receive 'MouseButtonState' changes to a 'MouseButton'.
setMouseButtonCallback :: MonadIO m => Maybe (MouseButton -> MouseButtonState -> ModifierKeys -> IO ()) -> ContextT WrappedWindow os f m ()
setMouseButtonCallback = wrapCallbackSetter GLFW.setMouseButtonCallback

-- | Poll for the 'MouseButtonState' of a 'MouseButton'.
getMouseButton :: MonadIO m => MouseButton -> ContextT WrappedWindow os f m MouseButtonState
getMouseButton = wrapWindowFun GLFW.getMouseButton

-- | Polling a 'MouseButton' for 'MouseButtonState' may sometimes miss state transitions.
-- If you use cannot use a callback to receive 'MouseButtonState' changes,
-- use 'getMouseButton' in combination with GLFW's sticky-mouse-buttons feature:
-- <http://www.glfw.org/docs/latest/input_guide.html#input_mouse_button>.
setStickyMouseButtonsInputMode :: MonadIO m => StickyMouseButtonsInputMode -> ContextT WrappedWindow os f m ()
setStickyMouseButtonsInputMode = wrapWindowFun GLFW.setStickyMouseButtonsInputMode

getStickyMouseButtonsInputMode :: MonadIO m => ContextT WrappedWindow os f m StickyMouseButtonsInputMode
getStickyMouseButtonsInputMode = withWindow GLFW.getStickyMouseButtonsInputMode

-- | Register or unregister a callback to receive scroll offset changes.
setScrollCallback :: MonadIO m => Maybe (Double -> Double -> IO ()) -> ContextT WrappedWindow os f m ()
setScrollCallback = wrapCallbackSetter GLFW.setScrollCallback

{- Joystick -}

{- Time -}

{- Clipboard -}

-- | Poll the system clipboard for a UTF-8 encoded string, if one can be extracted.
getClipboardString :: MonadIO m => ContextT WrappedWindow os f m (Maybe String)
getClipboardString = withWindow GLFW.getClipboardString

-- | Store a UTF-8 encoded string in the system clipboard.
setClipboardString :: MonadIO m => String -> ContextT WrappedWindow os f m ()
setClipboardString = wrapWindowFun GLFW.setClipboardString

{- Pathdrop -}

-- | Register or unregister a callback to receive file paths when files are dropped onto the window.
setDropCallback :: MonadIO m => Maybe ([String] -> IO ()) -> ContextT WrappedWindow os f m ()
setDropCallback = wrapCallbackSetter GLFW.setDropCallback
