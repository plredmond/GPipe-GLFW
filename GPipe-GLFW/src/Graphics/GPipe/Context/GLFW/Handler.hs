{-# LANGUAGE TypeFamilies #-} -- To define types in the ContextHandler instance
{-# LANGUAGE DeriveAnyClass #-} -- To derive 'Exception' w/o a standalone declaration.
{-# LANGUAGE TypeSynonymInstances #-} -- To derive 'Exception String'.
{-# LANGUAGE FlexibleInstances #-} -- To derive 'Exception String'.
-- | Internal module defining handler and its ContextHandler instance as well as some methods
module Graphics.GPipe.Context.GLFW.Handler where

-- stdlib
import Text.Printf (printf)
import Data.List (partition)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import Control.Monad (when, unless)
import Control.Exception (Exception, throwIO)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, modifyTVar)
import Control.Concurrent
    ( MVar, newMVar, modifyMVar, withMVar
    , ThreadId, myThreadId
    )
-- thirdparty
import Graphics.GPipe.Context (ContextHandler(..))
import qualified Graphics.UI.GLFW as GLFW (Window)
-- local
import qualified Graphics.GPipe.Context.GLFW.Calls as Call
import qualified Graphics.GPipe.Context.GLFW.Format as Format
import qualified Graphics.GPipe.Context.GLFW.RPC as RPC
import qualified Graphics.GPipe.Context.GLFW.Resource as Resource

bug :: String -> IO ()
bug s = Call.debug s >> throwIO s


data Window = Window
    { windowTid :: ThreadId
    , windowRaw :: GLFW.Window
    , windowHandler :: Handle
    }

-- | Opaque handle representing the initialized GLFW library.
data Handle = Handle
    { handleTid :: ThreadId
    , handleComm :: RPC.Handle
    , handleCtxs :: MVar [Window]
    }

-- REFACTOR:
-- * creating a context forks a thread where the context remains attached for its lifetime
-- * creating a context generates a window (int) which is passed back to gpipe
-- * all actions on a context by gpipe get RPC'd directly to the correct thread
--      (according to whether the glfw docs say it should run on the mainthread or context-thread)
-- * context threads run their own loop which is just an rpc receiver which can shut itself down
-- * handle runs in mainthread loop or mainstep

effectMain :: Handle -> Call.EffectMain
effectMain handle = RPC.sendEffect (handleComm handle)

onMain :: Handle -> Call.OnMain a
onMain handle = RPC.fetchResult (handleComm handle)

instance ContextHandler Handle where
    type ContextHandlerParameters Handle = Resource.HandleConfig
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
    -- Create a context which shares objects with the first context created by
    -- this handle.
    createContext handle settings = do
        -- XXX: consider forking here and setting up the context to receive RPCs on its own thread
        if null disallowedHints
            then Call.windowHints (effectMain handle) hints
            else throwIO $ Format.UnsafeWindowHintsException disallowedHints
        tid <- myThreadId
        modifyMVar (handleCtxs handle) $ \ctxs -> do
            let parent = if null ctxs then Nothing else Just . windowRaw . last $ ctxs
            result <- Call.createWindow (onMain handle) width height title monitor parent
            window <- case (result, parent) of
                (Just win, _) -> return win
                (Nothing, Nothing) -> throwIO . CreateWindowException . show $ config
                (Nothing, Just _) -> throwIO . CreateSharedWindowException . show $ config
            -- XXX: Consider releasing the mutex earlier.
            Call.debug $ printf "contextCreate of %s" (show window)
            Call.makeContextCurrent . pure $ window
            mapM_ Call.swapInterval interval
            return $ let ctx = Window tid window handle
                in (ctx : ctxs, ctx)
        where
            config = fromMaybe (Resource.defaultWindowConfig "") (snd <$> settings)
            Resource.WindowConfig {Resource.configWidth=width, Resource.configHeight=height} = config
            Resource.WindowConfig _ _ title monitor _ interval = config
            (userHints, disallowedHints) = partition Format.allowedHint $ Resource.configHints config
            hints = userHints ++ Format.bitsToHints (fst <$> settings) ++ Format.unconditionalHints

    -- GPipe Docs:
    --
    -- Run an OpenGL IO action in this context, that doesn't return any value
    -- to the caller. This may be run after contextDelete or
    -- contextHandlerDelete has been called. The thread calling this may not be
    -- the same creating the context (for finalizers it is most definetly not).
    --
    -- This impl:
    --
    -- Do work with the given context by making it current on the thread. If
    -- no context is given, the thread's current context is left undisturbed.
    contextDoAsync _handle ctxHuh action = do
        -- FIXME: this allows gpipe to hold onto a context that may have
        -- already been deleted, and we'll blindly attempt to make it current
        -- here..  consider giving gpipe a context identifier and looking the
        -- context up before doing anything
        mapM_ (Call.makeContextCurrent . pure . windowRaw) ctxHuh
        action

    -- GPipe Docs:
    -- Swap the front and back buffers in the context's default frame buffer.
    -- Called from same thread as created context.
    --
    -- This impl:
    --
    -- Swap buffers for the given context. Calling thread isn't important.
    contextSwap _handle ctx = Call.swapBuffers $ windowRaw ctx

    -- GPipe Docs:
    -- Get the current size of the context's default framebuffer (which may
    -- change if the window is resized). Called from same thread as created
    -- context.
    --
    -- This impl:
    --
    -- Fetch framebuffer size for the given context by RPCing the main
    -- thread.
    contextFrameBufferSize handle ctx = Call.getFramebufferSize (onMain handle) $ windowRaw ctx

    -- GPipe Docs:
    -- Delete a context and close any associated window. Called from same
    -- thread as created the context.
    --
    -- This impl:
    --
    -- Destroy the given context by making it uncurrent from its thread and
    -- then RPCing the main thread.
    -- Note: See the restrictions for Call.destroyWindow
    contextDelete handle ctx = do
        Call.debug $ printf "contextDelete of %s" (show $ windowRaw ctx)
        -- TODO: need to remove the context from the list of contexts!
        Call.makeContextCurrent Nothing
        Call.destroyWindow (onMain handle) (windowRaw ctx)

    contextHandlerCreate config = do
        main <- myThreadId
        comm <- RPC.newBound
        ctxs <- newMVar []
        let handle = Handle main comm ctxs
        Call.setErrorCallback (onMain handle) $ Just errorHandler
        ok <- Call.init $ onMain handle
        unless ok $ throwIO InitException
        return handle
        where
            Resource.HandleConfig errorHandler = config

    contextHandlerDelete handle = do
        -- FIXME: clean up handleCtxs before doing this
        Call.terminate $ effectMain handle
        Call.setErrorCallback (onMain handle) Nothing

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
-- complex applications.
--
-- * Must be called on the main thread.
--
-- [eventPolicy] 'Poll' or 'Wait'. 'Poll' will process events and return
-- immediately while 'Wait' will sleep until events are received.
mainstep :: Window -> EventPolicy -> IO ()
mainstep ctx eventPolicy = do
    tid <- myThreadId
    when (tid /= handleTid handle)
        (bug "mainstep called on wrong thread")
    case eventPolicy of
        Poll -> Call.pollEvents id
        Wait -> withAsync (RPC.awaitActions (handleComm handle) >> Call.postEmptyEvent)
                            (const $ Call.waitEvents id)
    RPC.processActions $ handleComm handle
    where
        handle = windowHandler ctx

-- | Process GLFW and GPipe events according to the given 'EventPolicy' in a
-- loop until 'windowShouldClose' is true for /all/ of the windows associated
-- with the [handle]. Set 'windowShouldClose' with 'setWindowShouldClose'.
-- Call 'mainloop' for less complex applications or use 'mainstep' for a custom
-- loop.
--
-- * Must be called on the main thread.
--
-- [eventPolicy] 'Poll' or 'Wait'. A 'Poll' loop runs continuously while a
-- 'Wait' loop sleeps until events or user input occur.
mainloop :: Window -> EventPolicy -> IO ()
mainloop ctx eventPolicy = do
    mainstep ctx eventPolicy
    allShouldClose <- withMVar (handleCtxs . windowHandler $ ctx) $ \ctxs -> do
        and <$> mapM (Call.windowShouldClose . windowRaw) ctxs
    unless allShouldClose $ mainloop ctx eventPolicy

-- | IO exception thrown when GLFW library initialization fails.
data InitException = InitException
    deriving (Exception, Show)

-- | IO Exception thrown when GLFW window creation fails.
data CreateWindowException
    = CreateWindowException String
    | CreateSharedWindowException String
    deriving (Exception, Show)
instance Exception String
