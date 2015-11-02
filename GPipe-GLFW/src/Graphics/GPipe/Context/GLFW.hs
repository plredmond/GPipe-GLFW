{-# LANGUAGE RankNTypes, GADTs #-}
module Graphics.GPipe.Context.GLFW
( newContext,
  GLFWWindow(),
  getCursorPos, getMouseButton, getKey, windowShouldClose,
  MouseButtonState(..), MouseButton(..), KeyState(..), Key(..),
) where

import qualified Control.Concurrent as C
import qualified Graphics.GPipe.Context.GLFW.Format as Format
import qualified Graphics.GPipe.Context.GLFW.Resource as Resource
import qualified Graphics.GPipe.Context.GLFW.Util as Util
import qualified Graphics.UI.GLFW as GLFW (getCursorPos, getMouseButton, getKey, windowShouldClose, makeContextCurrent, destroyWindow, pollEvents)

import Control.Monad.IO.Class (MonadIO)
import Graphics.GPipe.Context (ContextFactory, ContextHandle(..),ContextT,withContextWindow)
import Graphics.UI.GLFW (MouseButtonState(..), MouseButton(..), KeyState(..), Key(..))
import Data.IORef
import Control.Monad (when)

data Message where
    ReqShutDown :: C.MVar () -> Message
    ReqExecuteSync :: forall a. IO a -> C.MVar a -> Message
    ReqExecuteAsync :: IO () -> Message

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
    h <- createContext msgC Nothing fmt
    contextDoAsync h True (return ()) -- First action on render thread: Just make window current
    return h

createContext :: C.Chan Message -> Maybe Resource.Window -> ContextFactory c ds GLFWWindow
createContext msgC share fmt = do
    w <- makeContext share
    GLFW.makeContextCurrent Nothing
    alive <- newIORef True -- This will always be used from render thread so no need to synchronize
    return ContextHandle
        { newSharedContext = mainthreadDoWhileContextUncurrent msgC w . createContext msgC (Just w) -- Create context on this thread while parent is uncurrent, then make parent current
        , contextDoSync = contextDoSyncImpl w msgC
        , contextDoAsync = contextDoAsyncImpl alive w msgC
        , contextSwap = contextDoSyncImpl w msgC False $ Util.swapBuffers w -- explicitly do it on the render thread to sync properly, GLFW allows this
        , contextFrameBufferSize = Util.getFramebufferSize w -- Runs on mainthread
        , contextDelete = case share of
            Nothing -> do contextDeleteImpl msgC -- This return when render thread is uncurrent and is shutting down (cannot serve any finalizers)
                          GLFW.destroyWindow w
            Just parentW  -> mainthreadDoWhileContextUncurrent msgC parentW (writeIORef alive False >> GLFW.destroyWindow w) -- Shared contexts still alive, delete while uncurrent, then make parent win current
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
        ReqShutDown reply -> GLFW.makeContextCurrent Nothing >> C.putMVar reply ()
        ReqExecuteSync action reply -> action >>= C.putMVar reply >> loop msgC
        ReqExecuteAsync action -> action >> loop msgC

------------------------------------------------------------------------------
-- Application rpc calls

-- Await sychronous concurrent IO from the OpenGL context thread
contextDoSyncImpl :: Resource.Window -> C.Chan Message -> Bool -> IO a -> IO a
contextDoSyncImpl w msgC inwin action = do
    reply <- C.newEmptyMVar
    C.writeChan msgC $ ReqExecuteSync (do when inwin (GLFW.makeContextCurrent (Just w))
                                          action)
                                      reply
    GLFW.pollEvents -- Ugly hack, but at least every swapContextBuffers will run this
    C.takeMVar reply

-- Dispatch asychronous concurrent IO to the OpenGL context thread
contextDoAsyncImpl :: IORef Bool -> Resource.Window -> C.Chan Message -> Bool -> IO () -> IO ()
contextDoAsyncImpl alive w msgC inwin action =
    C.writeChan msgC $ ReqExecuteAsync $ if inwin
                                            then do -- If needed to be run in this window, then only do it if window still exists
                                                alive' <- readIORef alive
                                                when alive' $ do
                                                        GLFW.makeContextCurrent (Just w)
                                                        action
                                            else
                                                action

-- Do action while renderhtread is uncurrent
mainthreadDoWhileContextUncurrent :: C.Chan Message -> Resource.Window -> IO a -> IO a
mainthreadDoWhileContextUncurrent msgC w mainAction = do
    syncMainWait <- C.newEmptyMVar
    syncRendWait <- C.newEmptyMVar
    let m = do GLFW.makeContextCurrent Nothing
               C.putMVar syncMainWait ()
               C.takeMVar syncRendWait -- Stop other async code from making window current (e.g. finalizers)
               GLFW.makeContextCurrent (Just w)

    C.writeChan msgC $ ReqExecuteAsync m
    C.takeMVar syncMainWait -- Wait for render thread to make window uncurrent
    ret <- mainAction
    C.putMVar syncRendWait () -- Release render thread
    return ret

-- Request that the OpenGL context thread shut down
contextDeleteImpl :: C.Chan Message -> IO ()
contextDeleteImpl msgC = do
    syncMainWait <- C.newEmptyMVar
    C.writeChan msgC $ ReqShutDown syncMainWait
    C.takeMVar syncMainWait

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
