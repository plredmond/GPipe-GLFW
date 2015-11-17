-- | Window actions, mostly corresponding to user input. Analogous to those from
-- 'Graphics.UI.GLFW', but in the GPipe 'ContextT' monad.
module Graphics.GPipe.Context.GLFW.Input
(
  -- * Exposed actions
  getCursorPos,
  getMouseButton,
  getKey,
  windowShouldClose,
  -- * Re-exported from GLFW
  MouseButtonState(..), MouseButton(..), KeyState(..), Key(..)
) where

import Graphics.GPipe.Context.GLFW.Unsafe (GLFWWindow(..))

import Control.Monad.IO.Class (MonadIO)
import Graphics.GPipe.Context (ContextT, withContextWindow)
import qualified Graphics.UI.GLFW as GLFW (getCursorPos, getMouseButton, getKey, windowShouldClose)
import Graphics.UI.GLFW (MouseButtonState(..), MouseButton(..), KeyState(..), Key(..))

-- | Gets the current cursor position, in pixels relative to the top-left corner
-- of the window.
getCursorPos :: MonadIO m => ContextT GLFWWindow os f m (Double, Double)
getCursorPos = withContextWindow (GLFW.getCursorPos . getGLFWWindow)

-- | Gets the state of the specified 'MouseButton'.
getMouseButton :: MonadIO m => MouseButton -> ContextT GLFWWindow os f m MouseButtonState
getMouseButton mb = withContextWindow (\(GLFWWindow w) -> GLFW.getMouseButton w mb)

-- | Gets the state of the specified 'Key'.
getKey :: MonadIO m => Key -> ContextT GLFWWindow os f m KeyState
getKey k = withContextWindow (\(GLFWWindow w) -> GLFW.getKey w k)

-- | Returns 'True' if the window should close (e.g. because the user pressed
-- the \'x\' button).
windowShouldClose :: MonadIO m => ContextT GLFWWindow os f m Bool
windowShouldClose = withContextWindow (GLFW.windowShouldClose . getGLFWWindow)
