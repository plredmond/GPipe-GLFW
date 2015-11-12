{-# LANGUAGE RankNTypes, GADTs #-}
module Graphics.GPipe.Context.GLFW
(
  -- * Creating contexts
  newContext,
  newContext',
  -- * Data types
  BadWindowHintsException(..),
  GLFWWindow(),
  WindowConf(..), defaultWindowConf,
  -- * Re-exported window actions
  module Input
) where

import qualified Graphics.GPipe.Context.GLFW.Format as Format
import Graphics.GPipe.Context.GLFW.Input as Input
import qualified Graphics.GPipe.Context.GLFW.Resource as Resource
import Graphics.GPipe.Context.GLFW.Resource (WindowConf, defaultWindowConf, GLFWWindow(..))
import qualified Graphics.GPipe.Context.GLFW.Util as Util

import qualified Control.Concurrent as C
import qualified Graphics.UI.GLFW as GLFW (makeContextCurrent, destroyWindow, pollEvents)
import Graphics.GPipe.Context (ContextFactory, ContextHandle(..))
import Graphics.UI.GLFW (WindowHint(..))
import Data.IORef
import Control.Monad (when, unless)
import Control.Exception (Exception, throwIO)

data Message where
    ReqShutDown :: C.MVar () -> Message
    ReqExecuteSync :: forall a. IO a -> C.MVar a -> Message
    ReqExecuteAsync :: IO () -> Message

------------------------------------------------------------------------------
-- Top-level

-- | An exception which is thrown when you try to use 'WindowHint's that need to
-- be controlled by this library. Contains a list of the offending hints.
data BadWindowHintsException = BadWindowHintsException [WindowHint]
                                deriving (Show)

instance Exception BadWindowHintsException

-- | The context factory which facilitates use of GLFW with GPipe.
-- This has to be run from the main thread.
newContext :: ContextFactory c ds GLFWWindow
newContext = newContext' [] defaultWindowConf

-- | The context factory which facilitates use of GLFW with GPipe.
-- This has to be run from the main thread.
--
-- Accepts two extra parameters compared to 'newContext': a list of GLFW
-- 'WindowHint's and a 'WindowConf' which determines the width, height and title
-- of the window.
--
-- Throws a 'BadWindowHintsException' if you use hints that need to be
-- controlled by this library. Disallowed hints are:
--
-- > WindowHint'sRGBCapable
-- > WindowHint'Visible
-- > WindowHint'RedBits
-- > WindowHint'GreenBits
-- > WindowHint'BlueBits
-- > WindowHint'AlphaBits
-- > WindowHint'DepthBits
-- > WindowHint'StencilBits
-- > WindowHint'ContextVersionMajor
-- > WindowHint'ContextVersionMinor
-- > WindowHint'OpenGLForwardCompat
-- > WindowHint'OpenGLProfile
--
newContext' :: [WindowHint] -> WindowConf -> ContextFactory c ds GLFWWindow
newContext' extraHints conf fmt = do
    let badHints = filter (not . allowedHint) extraHints
    unless (null badHints) $
      throwIO (BadWindowHintsException badHints)

    chReply <- C.newEmptyMVar
    _ <- C.forkOS $ begin chReply
    msgC <- C.takeMVar chReply
    h <- createContext extraHints conf msgC Nothing fmt
    contextDoAsync h True (return ()) -- First action on render thread: Just make window current
    return h

createContext :: [WindowHint] -> WindowConf -> C.Chan Message -> Maybe Resource.Window -> ContextFactory c ds GLFWWindow
createContext extraHints conf msgC share fmt = do
    w <- makeContext share
    GLFW.makeContextCurrent Nothing
    alive <- newIORef True -- This will always be used from render thread so no need to synchronize
    return ContextHandle
        { newSharedContext = mainthreadDoWhileContextUncurrent msgC w . createContext extraHints conf msgC (Just w) -- Create context on this thread while parent is uncurrent, then make parent current
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
        hints = Format.toHints fmt ++ extraHints
        makeContext :: Maybe Resource.Window -> IO Resource.Window
        makeContext Nothing = Resource.newContext Nothing hints (Just conf)
        makeContext (Just s) = Resource.newSharedContext s hints (Just conf)

-- | Is the user allowed to use the given WindowHint?
allowedHint :: WindowHint -> Bool
allowedHint (WindowHint'sRGBCapable _) = False
allowedHint (WindowHint'Visible _) = False
allowedHint (WindowHint'RedBits _) = False
allowedHint (WindowHint'GreenBits _) = False
allowedHint (WindowHint'BlueBits _) = False
allowedHint (WindowHint'AlphaBits _) = False
allowedHint (WindowHint'DepthBits _) = False
allowedHint (WindowHint'StencilBits _) = False
allowedHint (WindowHint'ContextVersionMajor _) = False
allowedHint (WindowHint'ContextVersionMinor _) = False
allowedHint (WindowHint'OpenGLForwardCompat _) = False
allowedHint (WindowHint'OpenGLProfile _) = False
allowedHint _ = True

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

-- eof
