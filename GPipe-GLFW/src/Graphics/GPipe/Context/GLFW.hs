{-# LANGUAGE RankNTypes, GADTs #-}
module Graphics.GPipe.Context.GLFW
( newContext,
  GLFWWindow(),
  getCursorPos, getMouseButton, getKey, windowShouldClose,
  MouseButtonState(..), MouseButton(..), KeyState(..), Key(..),
) where

-- qualified
import qualified Control.Monad as M
import qualified Control.Concurrent as C
import qualified Graphics.GPipe.Context.GLFW.Resource as Resource
import qualified Graphics.GPipe.Context.GLFW.Util as Util
import qualified Graphics.GPipe.Context.GLFW.Format as Format
import qualified Graphics.UI.GLFW as GLFW (getCursorPos, getMouseButton, getKey, windowShouldClose, makeContextCurrent, destroyWindow)

-- unqualified
import Graphics.GPipe.Context (ContextFactory, ContextHandle(..),ContextT,withContextWindow)
import Graphics.GPipe.Format (ContextFormat)
import Graphics.UI.GLFW (MouseButtonState(..), MouseButton(..), KeyState(..), Key(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (isNothing)

type Message = Maybe Request

data Request where
    ReqExecute :: forall a. IO a -> Maybe (C.MVar a) -> Request

------------------------------------------------------------------------------
-- Top-level

newtype GLFWWindow = GLFWWindow { unGLFWWindow :: Resource.Window } 

newContext :: ContextFactory c ds GLFWWindow
newContext fmt = do
    chReply <- C.newEmptyMVar
    _ <- C.forkOS $ begin chReply fmt
    C.takeMVar chReply

createContext :: C.Chan Message -> Maybe (Resource.Window) -> ContextFactory c ds GLFWWindow
createContext msgC share fmt = do
    w <- makeContext share
    return ContextHandle
        { newSharedContext = contextDoSyncImpl w msgC . createContext msgC (Just w)
        , contextDoSync = contextDoSyncImpl w msgC
        , contextDoAsync = contextDoAsyncImpl w msgC
        , contextSwap = Util.swapBuffers w -- this thread only
        , contextFrameBufferSize = Util.getFramebufferSize w -- this thread only
        , contextDelete = do contextDoSyncImpl w msgC (GLFW.destroyWindow w)
                             M.when (isNothing share) $ contextDeleteImpl msgC -- Shut down thread when outermost shared context is destroyed                               
        , contextWindow = GLFWWindow w
        }
    where
        hints = Format.toHints fmt
        makeContext :: Maybe Resource.Window -> IO Resource.Window
        makeContext Nothing = Resource.newContext Nothing hints Nothing
        makeContext (Just s) = Resource.newSharedContext s hints Nothing
    
------------------------------------------------------------------------------
-- OpenGL Context thread

-- Create and pass back a ContextHandle. Enter loop.
begin :: C.MVar (ContextHandle GLFWWindow) -> ContextFormat c ds -> IO ()
begin chReply fmt = do msgC <- C.newChan
                       handle <- createContext msgC Nothing fmt
                       C.putMVar chReply handle
                       loop msgC

-- Handle messages until a stop message is received.
loop :: C.Chan Message -> IO ()
loop msgC = do
    msg <- C.readChan msgC
    case msg of
        Nothing -> return ()
        Just req -> doRequest req >> loop msgC

-- Do what the a request asks.
doRequest :: Request -> IO ()
doRequest (ReqExecute action Nothing) = M.void action
doRequest (ReqExecute action (Just reply)) = action >>= C.putMVar reply

------------------------------------------------------------------------------
-- Application rpc calls

-- Await sychronous concurrent IO from the OpenGL context thread
contextDoSyncImpl :: Resource.Window -> C.Chan Message -> IO a -> IO a
contextDoSyncImpl w msgC action = do
    reply <- C.newEmptyMVar
    C.writeChan msgC . Just $ ReqExecute (GLFW.makeContextCurrent (Just w) >> action) (Just reply)
    C.takeMVar reply

-- Dispatch asychronous concurrent IO to the OpenGL context thread
contextDoAsyncImpl :: Resource.Window -> C.Chan Message -> IO () -> IO ()
contextDoAsyncImpl w msgC action =
    C.writeChan msgC . Just $ ReqExecute (GLFW.makeContextCurrent (Just w) >> action) Nothing

-- Request that the OpenGL context thread shut down
contextDeleteImpl :: C.Chan Message -> IO ()
contextDeleteImpl msgC =
    C.writeChan msgC Nothing

------------------------------------------------------------------------------
-- Exposed window actions

getCursorPos :: MonadIO m => ContextT GLFWWindow os f m (Double, Double)
getCursorPos = withContextWindow (GLFW.getCursorPos . unGLFWWindow) 

getMouseButton :: MonadIO m => MouseButton -> ContextT GLFWWindow os f m MouseButtonState 
getMouseButton mb = withContextWindow (\(GLFWWindow w) -> GLFW.getMouseButton w mb) 

getKey :: MonadIO m => Key -> ContextT GLFWWindow os f m KeyState
getKey k = withContextWindow (\(GLFWWindow w) -> GLFW.getKey w k)

windowShouldClose :: MonadIO m => ContextT GLFWWindow os f m Bool
windowShouldClose = withContextWindow (GLFW.windowShouldClose . unGLFWWindow) 

-- eof
