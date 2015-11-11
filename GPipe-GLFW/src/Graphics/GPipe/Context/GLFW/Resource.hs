{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}
-- | Bracketed GLFW resource initializers.
module Graphics.GPipe.Context.GLFW.Resource
( newContext
, newSharedContext
, WindowConf(..)
, GLFWWindow(..)
, defaultWindowConf
, Window
, ErrorCallback
) where

import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import qualified Control.Exception as Exc
import qualified Data.Maybe as M
import qualified Text.Printf as P

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif

------------------------------------------------------------------------------
-- Types & Constants

-- | A value representing a GLFW OpenGL context window.
newtype GLFWWindow = GLFWWindow
  {
    -- | Gets the underlying 'GLFW.Window' object out of the 'GLFWWindow'.
    --
    -- Can be used inside a 'Graphics.GPipe.Context.ContextT' as follows:
    --
    -- > withContextWindow (\win -> doSomething (getGLFWWindow win))
    --
    -- WARNING: It is possible to do bad things with this. For example, using
    -- 'GLFW.makeContextCurrent' could cause GPipe to lose control of the window,
    -- and 'GLFW.destroyWindow' is bad for obvious reasons.
    getGLFWWindow :: GLFW.Window
  }

-- reexports
type Window = GLFW.Window
type ErrorCallback = GLFW.ErrorCallback

-- a default error callback which ragequits
defaultOnError :: ErrorCallback
defaultOnError err msg = fail $ P.printf "%s: %s" (show err) msg

-- | Initial window size and title suggestions for GLFW. The window will usually
-- be set to the given size with the given title, unless the window manager
-- overrides this.
data WindowConf = WindowConf
    { width :: Int
    , height :: Int
    , title :: String
    }

-- | A set of sensible defaults for the 'WindowConf'. Used by
-- 'Graphics.GPipe.Context.GLFW.newContext'.
defaultWindowConf :: WindowConf
defaultWindowConf = WindowConf 1024 768 "GLFW Window"

------------------------------------------------------------------------------
-- Code

-- set and unset the GLFW error callback, using a default if none is provided
withErrorCallback :: Maybe ErrorCallback -> IO a -> IO a
withErrorCallback customOnError =
    Exc.bracket_
        (GLFW.setErrorCallback $ Just onError)
        (GLFW.setErrorCallback Nothing)
    where
        onError :: ErrorCallback
        onError = M.fromMaybe defaultOnError customOnError

-- init and terminate GLFW
withGLFW :: IO a -> IO a
withGLFW =
    Exc.bracket_
        GLFW.init
        $ return () -- GLFW.terminate
        -- to clean up we should call GLFW.terminate, but it currently breaks
        -- see issue https://github.com/bsl/GLFW-b/issues/54

-- reset window hints and apply the given list, afterward reset window hints
withHints :: [GLFW.WindowHint] -> IO a -> IO a
withHints hints =
    Exc.bracket_
        (GLFW.defaultWindowHints >> mapM_ GLFW.windowHint hints)
        GLFW.defaultWindowHints

-- create a window, as the current context, using any monitor
-- if given a `Window`, create the new window's context from that
newWindow :: Maybe Window -> Maybe WindowConf -> IO Window
newWindow share customWindowConf =
    M.fromMaybe noWindow <$> createWindowHuh
    where
        WindowConf {width=w, height=h, title=t} = M.fromMaybe defaultWindowConf customWindowConf
        createWindowHuh :: IO (Maybe Window)
        createWindowHuh = do
            GLFW.makeContextCurrent Nothing
            win <- GLFW.createWindow w h t Nothing share
            GLFW.makeContextCurrent win
            return win
        noWindow :: Window
        noWindow = error "Couldn't create a window"

------------------------------------------------------------------------------
-- Top-level

-- establish a *new* opengl context
newContext :: Maybe ErrorCallback -> [GLFW.WindowHint] -> Maybe WindowConf -> IO Window
newContext ec hints wc
    = withErrorCallback ec
    . withGLFW
    . withHints hints
    $ newWindow Nothing wc

-- establish a *shared* opengl context
newSharedContext :: Window -> [GLFW.WindowHint] -> Maybe WindowConf -> IO Window
newSharedContext ctx hints wc
    = withHints hints
    $ newWindow (Just ctx) wc

-- eof
