{-# LANGUAGE RankNTypes, GADTs #-}
module Graphics.GPipe.Context.GLFW
( newContext,
  GLFWWindow(),
  getCursorPos, getMouseButton, getKey, windowShouldClose,
  MouseButtonState(..), MouseButton(..), KeyState(..), Key(..),
) where

import qualified Control.Concurrent as C
import qualified Control.Monad as M
import qualified Graphics.GPipe.Context.GLFW.Format as Format
import qualified Graphics.GPipe.Context.GLFW.Resource as Resource
import qualified Graphics.GPipe.Context.GLFW.Util as Util
import qualified Graphics.UI.GLFW as GLFW (getCursorPos, getMouseButton, getKey, windowShouldClose, makeContextCurrent, destroyWindow, pollEvents)

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (isNothing)
import Graphics.GPipe.Context (ContextFactory, ContextHandle(..),ContextT,withContextWindow)
import Graphics.GPipe.Format (ContextFormat)
import Graphics.UI.GLFW (MouseButtonState(..), MouseButton(..), KeyState(..), Key(..))

type Message = Maybe Request

data Request where
    ReqExecute :: forall a. IO a -> Maybe (C.MVar a) -> Request

------------------------------------------------------------------------------
-- Top-level

-- | An opaque value representing a GLFW OpenGL context window.
newtype GLFWWindow = GLFWWindow { unGLFWWindow :: Resource.Window }

-- | The context factory which facilitates use of GLFW with GPipe.
--   This has to be run from the main thread.
newContext :: ContextFactory c ds GLFWWindow
newContext fmt = do
    chReply <- C.newEmptyMVar
    _ <- C.forkOS $ begin chReply
    msgC <- C.takeMVar chReply
    createContext msgC Nothing fmt    

createContext :: C.Chan Message -> Maybe Resource.Window -> ContextFactory c ds GLFWWindow
createContext msgC share fmt = do
    w <- makeContext share
    GLFW.makeContextCurrent Nothing
    return ContextHandle
        { newSharedContext = \ fmt -> do 
                                 syncMainWait <- C.newEmptyMVar
                                 syncRendWait <- C.newEmptyMVar
                                 contextDoAsyncImpl w msgC $ do GLFW.makeContextCurrent Nothing
                                                                C.putMVar syncMainWait ()                                                                    
                                                                C.takeMVar syncRendWait -- Stop other async code from making window current (e.g. finalizers)
                                 C.takeMVar syncMainWait -- Wait for render thread to make window uncurrent
                                 handle <- createContext msgC (Just w) fmt
                                 C.putMVar syncRendWait () -- Release render thread
                                 return handle
        , contextDoSync = contextDoSyncImpl w msgC
        , contextDoAsync = contextDoAsyncImpl w msgC
        , contextSwap = Util.swapBuffers w -- this thread only
        , contextFrameBufferSize = Util.getFramebufferSize w -- this thread only
        , contextDelete = do
            contextDoSyncImpl w msgC (GLFW.destroyWindow w)
            -- Shut down thread when outermost shared context is destroyed
            M.when (isNothing share) $ contextDeleteImpl msgC
        , contextWindow = GLFWWindow w
        }
    where
        hints = Format.toHints fmt
        makeContext :: Maybe Resource.Window -> IO Resource.Window
        makeContext Nothing = Resource.newContext Nothing hints Nothing
        makeContext (Just s) = Resource.newSharedContext s hints Nothing

------------------------------------------------------------------------------
-- OpenGL Context thread

-- Create and pass back a channel. Enter loop.
begin :: C.MVar (C.Chan Message) ->  IO ()
begin chReply = do
    msgC <- C.newChan
    C.putMVar chReply msgC
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
    GLFW.pollEvents -- Ugly hack, but at least every swapContextBuffers will run this 
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
