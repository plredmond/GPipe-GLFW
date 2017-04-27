-- | Wrapped calls to GLFW-b for application programmer use. For user input see
-- "Graphics.GPipe.Context.GLFW.Input".
--
-- Actions are in the GPipe 'GPipe.ContextT' monad when a window handle is required,
-- otherwise they are bare reexported IO actions which can be lifted into the 'GPipe.ContextT' monad.
-- The 'Window' taken by many of these functions is the window resource from GPipe.
module Graphics.GPipe.Context.GLFW.Wrapped where

-- stdlib
import Control.Monad.IO.Class (MonadIO)
-- thirdparty
import qualified Graphics.GPipe.Context as GPipe (ContextT, Window())
-- local
import Graphics.GPipe.Context.GLFW.Handler (Handle(..))
import qualified Graphics.GPipe.Context.GLFW.Calls as Call
import qualified Graphics.GPipe.Context.GLFW.Wrappers as Wrappers

windowShouldClose :: MonadIO m => GPipe.Window os c ds -> GPipe.ContextT Handle os m (Maybe Bool)
windowShouldClose = Wrappers.withWindow Call.windowShouldClose

--TODO: setWindowShouldClose
--TODO: WindowHint, and associated methods
--TODO: Monitor, and associated methods
--TODO: ErrorCallback
