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

data Message = M'Stop | M'Request Request

data Request where
    R'Execute :: forall a. IO a -> Maybe (C.Chan a) -> Request
    R'NewSharedContext :: Request -- TODO

------------------------------------------------------------------------------
-- Interface

newContext :: ContextFactory c ds
newContext contextFormat = do
    handleReply <- C.newEmptyMVar
    -- TODO: examine contextFormat to setup framebuffer
    _ <- C.forkIO . Internal.withGL Nothing Nothing $ \w -> do
        msgC <- C.newChan
        C.putMVar handleReply ContextHandle
            { newSharedContext = undefined -- TODO
            , contextDoSync = contextDoSyncImpl msgC
            , contextDoAsync = contextDoAsyncImpl msgC
            , contextSwap = Internal.swapBuffers w -- this thread only
            , contextFrameBufferSize = Internal.getFramebufferSize w -- this thread only
            , contextDelete = contextDeleteImpl msgC
            }
        loop msgC
    C.takeMVar handleReply

------------------------------------------------------------------------------
-- OpenGL Context thread

-- Handle messages until a stop message is received.
loop :: C.Chan Message -> IO ()
loop msgC = do
    msg <- C.readChan msgC
    case msg of
        M'Stop -> return ()
        M'Request r -> request r >> loop msgC

-- Do what the a request asks.
request :: Request -> IO ()
request (R'Execute action Nothing) = M.void action
request (R'Execute action (Just replyC)) = action >>= C.writeChan replyC
request R'NewSharedContext = undefined -- TODO

------------------------------------------------------------------------------
-- Application rpc calls

-- Await sychronous concurrent IO from the OpenGL context thread
contextDoSyncImpl :: C.Chan Message -> IO a -> IO a
contextDoSyncImpl msgC action = do
    replyC <- C.newChan
    C.writeChan msgC . M'Request $ R'Execute action (Just replyC)
    C.readChan replyC

-- Dispatch asychronous concurrent IO to the OpenGL context thread
contextDoAsyncImpl :: C.Chan Message -> IO () -> IO ()
contextDoAsyncImpl msgC action =
    C.writeChan msgC . M'Request $ R'Execute action Nothing

-- Request that the OpenGL context thread shut down
contextDeleteImpl :: C.Chan Message -> IO ()
contextDeleteImpl msgC =
    C.writeChan msgC M'Stop

-- eof
