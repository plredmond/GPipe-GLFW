{-# LANGUAGE RankNTypes, GADTs #-}
module Graphics.GPipe.Context.GLFW
( newContext
) where

-- qualified
import qualified Control.Monad as M
import qualified Control.Concurrent as C
import qualified Graphics.GPipe.Context.GLFW.Internal as Internal

-- unqualified
import Graphics.GPipe.Context (ContextFactory, ContextHandle(..))

type Message = Maybe Request

data Request where
    ReqExecute :: forall a. IO a -> Maybe (C.MVar a) -> Request

------------------------------------------------------------------------------
-- Top-level

newContext :: ContextFactory c ds
newContext = context Nothing

context :: Maybe Internal.Window -> ContextFactory c ds
context share contextFormat = do
    chReply <- C.newEmptyMVar
    _ <- C.forkOS . withContext share $ \w -> do
        msgC <- C.newChan
        C.putMVar chReply ContextHandle
            { newSharedContext = context $ Just w
            , contextDoSync = contextDoSyncImpl msgC
            , contextDoAsync = contextDoAsyncImpl msgC
            , contextSwap = Internal.swapBuffers w -- this thread only
            , contextFrameBufferSize = Internal.getFramebufferSize w -- this thread only
            , contextDelete = contextDeleteImpl msgC
            }
        loop msgC
    C.takeMVar chReply
    where
        -- TODO: examine contextFormat to set up framebuffer
        withContext :: Maybe Internal.Window -> (Internal.Window -> IO a) -> IO a
        withContext Nothing = Internal.withNewContext Nothing Nothing
        withContext (Just w) = Internal.withSharedContext w Nothing

------------------------------------------------------------------------------
-- OpenGL Context thread

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
