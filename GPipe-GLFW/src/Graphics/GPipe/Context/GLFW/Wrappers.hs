-- | Internal module supporting "Graphics.GPipe.Context.GLFW.Input" and "Graphics.GPipe.Context.GLFW.Wrapped"
module Graphics.GPipe.Context.GLFW.Wrappers where

-- stdlib
import Control.Monad.IO.Class (MonadIO)
-- thirdparty
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.GPipe.Context as GPipe (ContextT, withContextWindow, Window())
-- local
import qualified Graphics.GPipe.Context.GLFW.Handler as Handler (Handle(..), Context(..), withContextWindow, Window(..))
import qualified Graphics.GPipe.Context.GLFW.Calls as Call
import qualified Graphics.GPipe.Context.GLFW.RPC as RPC

-- | Convenience funcion to run the action with the context if GPipe can locate it and it is still open.
withWindowSimple :: MonadIO m => (GLFW.Window -> IO a) -> GPipe.Window os c ds -> GPipe.ContextT Handler.Handle os m (Maybe a)
withWindowSimple fun wid = Handler.withContextWindow "withWindowNoRPC" wid go
    where
        go _handle = fun . Handler.contextRaw

-- | Convenience function to look up and unwrap the GLFW window and route the GLFW function through RPC.
withWindow :: MonadIO m => (Call.OnMain a -> GLFW.Window -> IO a) -> GPipe.Window os c ds -> GPipe.ContextT Handler.Handle os m (Maybe a)
withWindow fun wid = Handler.withContextWindow "withWindowRPC" wid go
    where
        go handle = fun (RPC.fetchResult . Handler.handleComm $ handle) . Handler.contextRaw

-- | Convenience function to wrap two-argument functions taking window and something else.
wrapWindowFun :: MonadIO m => (Call.OnMain b -> GLFW.Window -> a -> IO b) -> GPipe.Window os c ds -> a -> GPipe.ContextT Handler.Handle os m (Maybe b)
wrapWindowFun fun wid x = flip withWindow wid $ \onMain window -> fun onMain window x

-- | Convenience function to wrap callback setters which take window and pass it to the callback.
-- Callbacks will be passed the GPipe window id.
wrapCallbackSetter :: (MonadIO m, Functor g) => (Call.OnMain a -> GLFW.Window -> g (GLFW.Window -> b) -> IO a) -> GPipe.Window os c ds -> g b -> GPipe.ContextT Handler.Handle os m (Maybe a)
wrapCallbackSetter setter wid cb = flip withWindow wid $ \onMain window -> setter onMain window (const <$> cb)

