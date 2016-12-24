{-# LANGUAGE DeriveAnyClass #-} -- To derive 'Exception' w/o a standalone declaration.
{-# LANGUAGE TypeSynonymInstances #-} -- To derive 'Exception String'.
{-# LANGUAGE FlexibleInstances #-} -- To derive 'Exception String'.
-- | Bracketed GLFW resource initializers.
module Graphics.GPipe.Context.GLFW.Resource
( withContext
, withSharedContext
, WindowConfig(..)
, EventPolicy(..)
, WrappedWindow(..)
) where

-- stdlib
import Control.Exception
    ( Exception, throwIO
    )
import Control.Monad.Exception
    ( bracket, bracket_
    , MonadAsyncException()
    )
import Control.Monad.IO.Class (liftIO)

-- third party
import qualified Graphics.UI.GLFW as GLFW

-- defined here so other internal modules (eg. Input) can get at the unwrapper function
newtype WrappedWindow = WrappedWindow
    { rawContext :: GLFW.Window
    }

data EventPolicy
    = Poll
    | Wait
    deriving
    ( Show
    )

data WindowConfig = WindowConfig
    { wc'width :: Int
    , wc'height :: Int
    , wc'title :: String
    , wc'eventPolicy :: EventPolicy
    , wc'monitor :: Maybe GLFW.Monitor
    , wc'hints :: [GLFW.WindowHint]
    } deriving
    ( Show
    )

-- establish and teardown a new opengl context
withContext :: (MonadAsyncException m) => (IO W -> IO W) -> GLFW.ErrorCallback -> WindowConfig -> (GLFW.Window -> m a) -> m a
withContext inject ec wc
    = withErrorCallback ec
    . withGLFW
    . withHints (wc'hints wc)
    . withWindow inject Nothing wc

-- establish and teardown a shared opengl context
withSharedContext :: (MonadAsyncException m) => (IO W -> IO W) -> GLFW.Window -> WindowConfig -> (GLFW.Window -> m a) -> m a
withSharedContext inject ctx wc
    = withHints (wc'hints wc)
    . withWindow inject (Just ctx) wc

data GLFWCreateWindowException
    = GLFWCreateWindowException String
    | GLFWCreateSharedWindowException String
    deriving
    ( Exception
    , Show
    )
instance Exception String

-- create a context in glfw, afterward destroy it
-- call inject with the IO action that creates the window (allows, eg. creating the window on a different thread)
newtype W = Opaque (Maybe (GLFW.Window)) -- inject gets (IO W) which allows it to either run the IO action or pass it through--nothing more
-- after calling the inject function, examine the result and throw if no window was created
-- if given an existing context, the context created by this function is a shared context
withWindow :: (MonadAsyncException m) => (IO W -> IO W) -> Maybe GLFW.Window -> WindowConfig -> (GLFW.Window -> m a) -> m a
withWindow inject parent wc = bracket
        (liftIO $ inject create >>= unwrap)
        (liftIO . GLFW.destroyWindow)
    where
        WindowConfig {wc'width=w, wc'height=h, wc'title=t, wc'monitor=m} = wc

        create = Opaque <$> GLFW.createWindow w h t m parent

        unwrap (Opaque mwin) = case (mwin, parent) of
                (Just win, _) -> return win
                (Nothing, Nothing) -> throwIO . GLFWCreateWindowException . show $ wc
                (Nothing, Just _) -> throwIO . GLFWCreateSharedWindowException . show $ wc

-- reset window hints and apply the given list, afterward reset window hints
withHints :: (MonadAsyncException m) => [GLFW.WindowHint] -> m a -> m a
withHints hints = bracket_
    (liftIO $ GLFW.defaultWindowHints >> mapM_ GLFW.windowHint hints)
    (liftIO GLFW.defaultWindowHints)

-- init and terminate GLFW
withGLFW :: (MonadAsyncException m) => m a -> m a
withGLFW = bracket_
    (liftIO GLFW.init)
    (liftIO GLFW.terminate)

-- set and unset the GLFW error callback
withErrorCallback :: (MonadAsyncException m) => GLFW.ErrorCallback -> m a -> m a
withErrorCallback callback = bracket_
    (liftIO . GLFW.setErrorCallback $ Just callback)
    (liftIO $ GLFW.setErrorCallback Nothing)
