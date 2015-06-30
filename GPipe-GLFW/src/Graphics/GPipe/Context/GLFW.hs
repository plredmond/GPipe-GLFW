module Graphics.GPipe.Context.GLFW where

-- qualified
import qualified Text.Printf as P
-- import qualified Data.Maybe as M
import qualified Control.Monad as M
-- import qualified Control.Exception as Exc
import qualified Control.Concurrent as C
import qualified Graphics.GPipe.Context.GLFW.Internal as Internal

-- unqualified
-- import Control.Applicative ((<$>))
import Graphics.GPipe.Format (ContextFormat)
import Graphics.GPipe.Context (ContextFactory, ContextHandle(..))

------------------------------------------------------------------------------
-- Top level

newContext :: ContextFactory c ds
newContext contextFormat = do
    msgC <- C.newChan
    _ <- C.forkIO $ do
        P.printf "Thread beginning\n"
        -- TODO: inform these arguments with the contextFormat
        Internal.withGL Nothing Nothing $ contextLoop msgC
        P.printf "Thread ending\n"
        return ()
    return ContextHandle
        { newSharedContext = undefined {- \cf -> do
            (NewSharedContextRet ch) <- await msgC (NewSharedContextMsg cf)
            return ch -} -- FIXME
        , contextDoSync = undefined {- \action -> do
            (ContextDoSyncRet r) <- await msgC (ContextDoSyncMsg action)
            return r -} -- FIXME
        , contextDoAsync = dispatch msgC . ContextDoAsyncMsg
        , contextSwap = M.void $ await msgC ContextSwapMsg
        , contextFrameBufferSize = do
            (ContextFrameBufferSizeRet size) <- await msgC ContextSwapMsg
            return size
        , contextDelete = M.void $ await msgC ContextDeleteMsg
        }

contextLoop :: C.Chan (ContextFactoryMsg c ds a, C.Chan (ContextFactoryRet a))
    -> Internal.Window
    -> IO ()
contextLoop msgC w = do
    signal <- turnaround msgC $ contextStep w
    M.when signal $ contextLoop msgC w

contextStep :: Internal.Window -> ContextFactoryMsg c ds a -> IO (ContextFactoryRet a, Bool)
contextStep w msg = do
    P.printf "Context thread got a message\n"
    case msg of
        NewSharedContextMsg contextFormat -> do
            -- TODO: create a new context handle to this context and reply with it
            reply $ NewSharedContextRet undefined
        ContextDoSyncMsg action -> do
            r <- action
            reply $ ContextDoSyncRet r
        ContextDoAsyncMsg action -> do
            action
            ignore
        ContextSwapMsg -> do
            Internal.swapBuffers w
            ignore
        ContextFrameBufferSizeMsg -> do
            size <- Internal.getFramebufferSize w
            reply $ ContextFrameBufferSizeRet size
        ContextDeleteMsg -> stop
    where
        reply x = return (x, True)
        ignore = return (OtherRet, True)
        stop = return (OtherRet, False)

------------------------------------------------------------------------------
-- Types & Constants

-- Self-Addressed Envelope
type SAE a b = C.Chan (a, C.Chan b)

-- RPC and wait for result
await :: SAE a b -> a -> IO b
await msgC msg = do
    replyC <- C.newChan
    C.writeChan msgC (msg, replyC)
    C.readChan replyC

-- RPC and ignore result
dispatch :: SAE a b -> a -> IO ()
dispatch msgC msg = do
    ignoredC <- C.newChan
    C.writeChan msgC (msg, ignoredC)

-- Handle RPC
turnaround :: SAE a b -> (a -> IO (b, c)) -> IO c
turnaround msgC handler = do
    (msg, replyC) <- C.readChan msgC
    (reply, sig) <- handler msg
    C.writeChan replyC reply
    return sig

data ContextFactoryMsg c ds a
    = NewSharedContextMsg (ContextFormat c ds)
    | ContextDoSyncMsg (IO a)
    | ContextDoAsyncMsg (IO ())
    | ContextSwapMsg
    | ContextFrameBufferSizeMsg
    | ContextDeleteMsg

data ContextFactoryRet a
    = NewSharedContextRet ContextHandle
    | ContextDoSyncRet a
    | ContextFrameBufferSizeRet (Int, Int)
    | OtherRet

-- eof
