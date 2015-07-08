{-# LANGUAGE RankNTypes, GADTs #-}
module Graphics.GPipe.Context.GLFW
( newContext
) where

-- qualified
import qualified Control.Monad as M
import qualified Control.Concurrent as C
import qualified Graphics.GPipe.Context.GLFW.Resource as Resource
import qualified Graphics.GPipe.Context.GLFW.Util as Util
import qualified Graphics.GPipe.Context.GLFW.Format as Format

-- unqualified
import Graphics.GPipe.Context (ContextFactory, ContextHandle(..))

type Message = Maybe Request

data Request where
    ReqExecute :: forall a. IO a -> Maybe (C.MVar a) -> Request

------------------------------------------------------------------------------
-- Top-level

newContext :: ContextFactory c ds
newContext = context Nothing

context :: Maybe Resource.Window -> ContextFactory c ds
context share fmt = do
    chReply <- C.newEmptyMVar
    _ <- C.forkOS . withContext share $ begin chReply
    C.takeMVar chReply
    where
        withContext :: Maybe Resource.Window -> (Resource.Window -> IO a) -> IO a
        withContext Nothing = Resource.withNewContext Nothing Nothing
        withContext (Just s) = Resource.withSharedContext s Nothing

------------------------------------------------------------------------------
-- OpenGL Context thread

-- Create and pass back a ContextHandle. Enter loop.
begin :: C.MVar ContextHandle -> Resource.Window -> IO ()
begin chReply w = do
    msgC <- C.newChan
    C.putMVar chReply ContextHandle
        { newSharedContext = context $ Just w
        , contextDoSync = contextDoSyncImpl msgC
        , contextDoAsync = contextDoAsyncImpl msgC
        , contextSwap = Util.swapBuffers w -- this thread only
        , contextFrameBufferSize = Util.getFramebufferSize w -- this thread only
        , contextDelete = contextDeleteImpl msgC
        }
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
contextDoSyncImpl :: C.Chan Message -> IO a -> IO a
contextDoSyncImpl msgC action = do
    reply <- C.newEmptyMVar
    C.writeChan msgC . Just $ ReqExecute action (Just reply)
    C.takeMVar reply

-- Dispatch asychronous concurrent IO to the OpenGL context thread
contextDoAsyncImpl :: C.Chan Message -> IO () -> IO ()
contextDoAsyncImpl msgC action =
    C.writeChan msgC . Just $ ReqExecute action Nothing

-- Request that the OpenGL context thread shut down
contextDeleteImpl :: C.Chan Message -> IO ()
contextDeleteImpl msgC =
    C.writeChan msgC Nothing

-- eof
