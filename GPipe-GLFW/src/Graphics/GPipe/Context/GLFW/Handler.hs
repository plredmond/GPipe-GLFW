{-# LANGUAGE TypeFamilies #-} -- To define types in the ContextHandler instance
{-# LANGUAGE DeriveAnyClass #-} -- To derive 'Exception' w/o a standalone declaration.
{-# LANGUAGE TypeSynonymInstances #-} -- To derive 'Exception String'.
{-# LANGUAGE FlexibleInstances #-} -- To derive 'Exception String'.
-- | Internal module defining handler and its ContextHandler instance as well as some methods
module Graphics.GPipe.Context.GLFW.Handler where

-- stdlib
import Control.Monad (forM_, forM)
import Text.Printf (printf)
import Data.List (partition, delete)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe, isJust, isNothing)
import Control.Monad (when, unless, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (Exception, bracket, bracket_, throwIO, onException)
import Control.Concurrent.Async
    ( Async, asyncBound, withAsync
    )
import Control.Concurrent.STM (atomically, throwSTM)
import Control.Concurrent.STM.TVar
    ( TVar, newTVarIO, readTVar, readTVarIO, writeTVar, modifyTVar
    )
import Control.Concurrent
    ( MVar, newEmptyMVar, newMVar, modifyMVar_, withMVar, putMVar, takeMVar
    , ThreadId, myThreadId
    )
-- thirdparty
import qualified Graphics.GPipe.Context as GPipe (ContextHandler(..), Window(), ContextT(), withContextWindow)
import qualified Graphics.UI.GLFW as GLFW (Window, ErrorCallback)
-- local
import qualified Graphics.GPipe.Context.GLFW.Calls as Call
import qualified Graphics.GPipe.Context.GLFW.Format as Format
import qualified Graphics.GPipe.Context.GLFW.RPC as RPC
import qualified Graphics.GPipe.Context.GLFW.Resource as Resource

bug :: String -> IO ()
bug s = Call.debug s >> throwIO s

-- | Internal handle for a GPipe-created GLFW window/context
data Context = Context
    { contextRaw :: GLFW.Window
--  , contextComm :: RPC.Handle
--  , contextAsync :: Async ()
    }
-- | Closeable internal handle for 'Context'.
type MMContext = MVar (Maybe Context)

-- | Opaque handle representing the initialized GLFW library and single uncurrent anscestor context.
data Handle = Handle
    { handleTid :: ThreadId
    , handleComm :: RPC.Handle
    , handleCtxs :: TVar [MMContext]
    }

-- | Opaque handle representing a, possibly closed, internal 'Context'.
newtype Window = Window (MMContext, Handle)

-- | Run the action with the context /if the context is still open/.
withContext :: String -> MMContext -> (Context -> IO a) -> IO (Maybe a)
withContext callerTag mmContext action = withMVar mmContext go
    where
        go Nothing = Call.debug (printf "%s: GPipe-GLFW context already closed" callerTag) >> return Nothing
        go (Just context) = pure <$> action context

-- | Run the action with the context /if the context exists and is still open/.
withContextWindow :: MonadIO m => String -> GPipe.Window os c ds -> (Handle -> Context -> IO a) -> GPipe.ContextT Handle os m (Maybe a)
withContextWindow callerTag wid action = GPipe.withContextWindow wid go
    where
        go Nothing = Call.debug (printf "%s: GPipe had no such window" callerTag) >> return Nothing
        go (Just (Window (mmContext, handle))) = withContext callerTag mmContext $ action handle

-- FIXME: May cause crashes in windows because it requires parent contexts to be uncurrent.
-- | Run the action. If any open context is avaiable, take it and pass it into the action.
withAnyContext :: Handle -> (Maybe Context -> IO a) -> IO a
withAnyContext handle action = readTVarIO (handleCtxs handle) >>= go
    where
        go (mmContext:ctxs) = withContext "withAnyContext" mmContext (action . pure) >>= maybe (go ctxs) return
        go [] = action Nothing

effectMain :: Handle -> Call.EffectMain
effectMain handle = RPC.sendEffect (handleComm handle)

onMain :: Handle -> Call.OnMain a
onMain handle = RPC.fetchResult (handleComm handle)

-- | Default configuration which prints any errors that GLFW emits.
defaultHandleConfig :: GPipe.ContextHandlerParameters Handle
defaultHandleConfig = HandleConfig errorHandler
    where
        -- TODO: swap printf for logger
        -- TODO: accumulate channel errors in a list or channel in handle until they can be processed somewhere
        errorHandler err desc = printf "%s: %s\n" (show err) desc

instance GPipe.ContextHandler Handle where
    data ContextHandlerParameters Handle = HandleConfig
        { configErrorCallback :: GLFW.ErrorCallback
        }
    type ContextWindow Handle = Window
    type WindowParameters Handle = Resource.WindowConfig

    -- GPipe Docs:
    --
    -- Create a new context sharing all other contexts created by this
    -- ContextHandler. If the parameter is Nothing, a hidden off-screen context
    -- is created, otherwise creates a window with the provided window bits and
    -- implementation specific parameters
    --
    -- This impl:
    --
    -- Create a context which shares objects with the contexts created by this
    -- handle, if any.
    createContext handle settings = do
        unless (null disallowedHints) $
            throwIO $ Format.UnsafeWindowHintsException disallowedHints
        -- make a context
        window <- withAnyContext handle $ \parent -> do
            windowHuh <- Call.createWindow (onMain handle) width height title monitor hints (contextRaw <$> parent)
            Call.debug $ printf "contextCreate made %s -> parent %s" (show windowHuh) (show $ contextRaw <$> parent)
            case (windowHuh, parent) of
                (Just w, _) -> return w
                (Nothing, Just _) -> throwIO . CreateSharedWindowException . show $ config {Resource.configHints = hints}
                (Nothing, Nothing) -> throwIO . CreateWindowException . show $ config {Resource.configHints = hints}
        -- set up context
        forM_ intervalHuh $ \interval -> do
            Call.debug "some context shuffling for swapInterval {"
            Call.makeContextCurrent $ pure window
            Call.swapInterval interval
            Call.makeContextCurrent Nothing
            Call.debug "}"
        -- wrap up context
        mmContext <- newMVar . pure $ Context window
        atomically $ modifyTVar (handleCtxs handle) (mmContext :)
        return $ Window (mmContext, handle)
        where
            config = fromMaybe (Resource.defaultWindowConfig "") (snd <$> settings)
            Resource.WindowConfig {Resource.configWidth=width, Resource.configHeight=height} = config
            Resource.WindowConfig _ _ title monitor _ intervalHuh = config
            (userHints, disallowedHints) = partition Format.allowedHint $ Resource.configHints config
            hints = userHints ++ Format.bitsToHints (fst <$> settings) ++ Format.unconditionalHints

{- -- junk which might be part of this later, if we do 1:1 contexts and threads
     reply <- newEmptyMVar
     async <- asyncBound $ do
         comm <- RPC.newBound
         putMVar reply comm
         -- TODO: drop into a mainloop with closure: window, handle
         return ()
     -- XXX: waiting on this mvar lets us bail if there's an issue in the async
     comm <- takeMVar reply
     -- TODO: poll the async before returning the Context (not really necessary.. also racey)
-}

    -- GPipe Docs:
    --
    -- Run an OpenGL IO action in this context, that doesn't return any value
    -- to the caller. This may be run after contextDelete or
    -- contextHandlerDelete has been called. The thread calling this may not be
    -- the same creating the context (for finalizers it is most definetly not).
    --
    -- This impl:
    --
    -- Do work with the specified context by making it current.
    contextDoAsync _ Nothing _action = Call.debug "contextDoAsync called w/o context"
    contextDoAsync _ (Just (Window (mmContext, _))) action =
        void $ withContext "contextDoAsync" mmContext $ \context -> do
            Call.makeContextCurrent . pure . contextRaw $ context
            action

    -- GPipe Docs:
    -- Swap the front and back buffers in the context's default frame buffer.
    -- Called from same thread as created context.
    --
    -- This impl:
    --
    -- Swap buffers for the specified context. Calling thread isn't important.
    contextSwap _ (Window (mmContext, _)) = do
        void $ withContext "contextSwap" mmContext $ Call.swapBuffers . contextRaw

    -- GPipe Docs:
    -- Get the current size of the context's default framebuffer (which may
    -- change if the window is resized). Called from same thread as created
    -- context.
    --
    -- This impl:
    --
    -- Fetch framebuffer size for the specified context by RPCing the main thread.
    contextFrameBufferSize _ (Window (mmContext, handle)) = do
        result <- withContext "contextFrameBufferSize" mmContext $ \context -> do
            Call.getFramebufferSize (onMain handle) $ contextRaw context
        maybe fail return result
        where
            fail = do
                Call.debug $ printf "contextFrameBufferSize could not access context"
                return (0, 0)

    -- GPipe Docs:
    -- Delete a context and close any associated window. Called from same
    -- thread as created the context.
    --
    -- This impl:
    --
    -- Destroy the given context by making it current on the main thread and
    -- then deleting it there.
    --
    -- Note: See the restrictions for Call.destroyWindow
    --
    -- Seems like its ok to delete any of the shared contexts, per:
    -- https://khronos.org/registry/OpenGL/specs/gl/glspec45.core.pdf (Section 5.1.1)
    contextDelete _ (Window (mmContext, handle)) = do
        -- close the context mvar
        modifyMVar_ mmContext $ \mContext -> do
            Call.debug $ printf "contextDelete of %s" (show $ contextRaw <$> mContext)
            forM_ mContext $ \context -> RPC.sendEffect (handleComm handle) $ do
                Call.makeContextCurrent (pure $ contextRaw context)
                Call.destroyWindow id (contextRaw context) -- id RPC because this is in a mainthread RPC
            return Nothing
        -- remove the context from the handle
        atomically $ modifyTVar (handleCtxs handle) (delete mmContext)

    contextHandlerCreate config = do
        -- make handle resources
        tid <- myThreadId
        comm <- RPC.newBound
        ctxs <- newTVarIO []
        -- initialize glfw
        Call.setErrorCallback id $ pure errorHandler -- id RPC because contextHandlerCreate is called only on mainthread
        ok <- Call.init id -- id RPC because contextHandlerCreate is called only on mainthread
        unless ok $ throwIO InitException
        -- wrap up handle
        return $ Handle tid comm ctxs
        where
            HandleConfig errorHandler = config

    contextHandlerDelete handle = do
        Call.debug "contextHandlerDelete"
        ctxs <- readTVarIO $ handleCtxs handle
        forM_ ctxs $ \mmContext -> GPipe.contextDelete handle (Window (mmContext, handle))
        atomically $ writeTVar (handleCtxs handle) []
        -- all resources are released
        Call.terminate id -- id RPC because contextHandlerDelete is called only on mainthread
        Call.setErrorCallback id Nothing -- id RPC because contextHandlerDelete is called only on mainthread

-- | Type to describe the waiting or polling style of event processing
-- supported by GLFW. See the 'mainloop' and 'mainstep' docs for usage.
--
-- * Recommended reading: /Event Processing/ section of the GLFW /Input Guide/
-- at <http://www.glfw.org/docs/latest/input_guide.html#events>.
data EventPolicy
    = Poll
    | Wait
    deriving
    ( Show
    )

-- | Process GLFW and GPipe events according to the given 'EventPolicy'. Call
-- 'mainstep' as part of a custom engine loop or use 'mainloop' for less
-- complex applications. Can be called with /any/ window you've created.
--
-- * Must be called on the main thread.
mainstep :: Window -> EventPolicy -- ^ 'Poll' will process events and return immediately while 'Wait' will sleep until events are received.
    -> IO ()
mainstep (Window (mmContext, handle)) eventPolicy = do
    tid <- myThreadId
    when (tid /= handleTid handle)
        (bug "mainstep must be called from main thread")
    case eventPolicy of
        Poll -> Call.pollEvents id
        Wait -> withAsync (RPC.awaitActions (handleComm handle) >> Call.postEmptyEvent)
                    (const $ Call.waitEvents id)
    RPC.processActions $ handleComm handle

-- | Process GLFW and GPipe events according to the given 'EventPolicy' in a
-- loop until 'windowShouldClose' is true for the all 'Window's created by the
-- same 'ContextHandler', or the 'Window's have been destroyed.
--
-- Set 'windowShouldClose' with 'setWindowShouldClose'. Call 'mainloop' for
-- less complex applications or use 'mainstep' for a custom loop.
--
-- * Must be called on the main thread.
mainloop :: Window -> EventPolicy -- ^ A 'Poll' loop runs continuously while a 'Wait' loop sleeps until events or user input occur.
    -> IO ()
mainloop window@(Window (mmContext, handle)) eventPolicy = do
    mainstep window eventPolicy
    ctxs <- readTVarIO $ handleCtxs handle
    allShouldClose <- and <$> forM ctxs oneShouldClose
    unless allShouldClose $ mainloop window eventPolicy
    where
        oneShouldClose :: MMContext -> IO Bool
        oneShouldClose mmContext = do
            shouldCloseHuh <- withContext "oneShouldClose" mmContext $ Call.windowShouldClose . contextRaw
            return $ fromMaybe True shouldCloseHuh

-- | IO exception thrown when GLFW library initialization fails.
data InitException = InitException
    deriving (Exception, Show)

-- | IO Exception thrown when GLFW window creation fails.
data CreateWindowException
    = CreateWindowException String
    | CreateSharedWindowException String
    deriving (Exception, Show)
instance Exception String
