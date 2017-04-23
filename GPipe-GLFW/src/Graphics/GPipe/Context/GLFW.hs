-- | Single-threaded applications must call 'mainstep' in the main application
-- loop to process GPipe events which call GLFW functions.
--
-- Multi-threaded applications must use __bound__ threads (ie. 'forkOS', not
-- 'forkIO') and may call either 'mainstep' or 'mainloop' on the main thread to
-- process GPipe events which call GLFW functions. Rendering and resource
-- management threads that create contexts must perform all GPipe operations on
-- that same thread.
module Graphics.GPipe.Context.GLFW (
-- * GPipe context handler for GLFW
Handle,
Window,
mainloop,
mainstep,
-- ** Configuration
defaultHandleConfig,
WindowConfig(..),
defaultWindowConfig,
EventPolicy(..),
-- ** Exceptions
InitException(..),
CreateWindowException(..),
UnsafeWindowHintsException(..),
-- ** Reexports
module Graphics.GPipe.Context.GLFW.Input,
module Graphics.GPipe.Context.GLFW.Wrapped
) where

-- internal
import Graphics.GPipe.Context.GLFW.Format
import Graphics.GPipe.Context.GLFW.Resource
import Graphics.GPipe.Context.GLFW.Handler
-- reexports
import Graphics.GPipe.Context.GLFW.Input
import Graphics.GPipe.Context.GLFW.Wrapped
