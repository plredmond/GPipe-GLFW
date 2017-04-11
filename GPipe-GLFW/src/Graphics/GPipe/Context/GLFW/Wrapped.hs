-- | Wrapped calls to GLFW-b for application programmer use
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
windowShouldClose = Wrappers.withWindowSimple Call.windowShouldClose
