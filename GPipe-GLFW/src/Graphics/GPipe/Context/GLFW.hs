-- | Non interactive applications only need to pass configuration defined here
-- into GPipe's 'runContextT' and 'newWindow'.
--
-- For user input, see "Graphics.GPipe.Context.GLFW.Input". All other GLFW
-- functionality is being incrementally exposed in
-- "Graphics.GPipe.Context.GLFW.Wrapped".
module Graphics.GPipe.Context.GLFW (
-- * GPipe context handler for GLFW
Handle(),
GLFWWindow(),
-- ** Configuration
-- *** Defaults
defaultHandleConfig,
defaultWindowConfig,
-- *** Details
ContextHandlerParameters(HandleConfig, configErrorCallback, configEventPolicy),
-- | Configuration for the GLFW handle.
--
-- [@'HandleConfig'@] Constructor
--
-- [@'configErrorCallback' :: ErrorCallback@] Specify a callback to handle errors captured by GLFW.
--
-- [@'configEventPolicy' :: Maybe 'EventPolicy'@] Specify the 'EventPolicy' to use for automatic GLFW event processing. If 'Nothing' then automatic event processing is disabled and you'll need to call 'mainloop' or 'mainstep' somewhere.
WindowConfig(..),
EventPolicy(..),
-- ** Exceptions
InitException(..),
CreateWindowException(..),
UnsafeWindowHintsException(..),
-- ** Mainthread hooks
mainloop,
mainstep,
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
