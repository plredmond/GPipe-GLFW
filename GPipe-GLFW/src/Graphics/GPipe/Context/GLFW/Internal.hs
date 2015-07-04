{-# LANGUAGE PackageImports #-}
module Graphics.GPipe.Context.GLFW.Internal
( withGL
, Window
, ErrorCallback
, swapBuffers
, getFramebufferSize
) where

-- qualified
import qualified Text.Printf as P
import qualified Data.Maybe as M
import qualified Control.Exception as Exc
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW

-- unqualified
import Control.Applicative ((<$>))

------------------------------------------------------------------------------
-- Types & Constants

-- reexports
type Window = GLFW.Window
type ErrorCallback = GLFW.ErrorCallback

-- a default error callback which ragequits
defaultOnError :: ErrorCallback
defaultOnError err msg = fail $ P.printf "%s: %s" (show err) msg

-- initial window size & title suggestions
data WindowConf = WindowConf
    { width :: Int
    , height :: Int
    , title :: String 
    }

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
        GLFW.terminate

-- create and destroy a new window, as the current context, using any monitor
withNewWindow :: Maybe WindowConf -> (Window -> IO a) -> IO a
withNewWindow customWindowConf =
    Exc.bracket
        createWindow
        GLFW.destroyWindow
    where
        WindowConf w h t = M.fromMaybe defaultWindowConf customWindowConf
        createWindowHuh :: IO (Maybe Window)
        createWindowHuh = do
            win <- GLFW.createWindow w h t Nothing Nothing
            GLFW.makeContextCurrent win
            return win
        noWindow :: Window
        noWindow = error "Couldn't create a window"
        createWindow :: IO Window
        createWindow = M.fromMaybe noWindow <$> createWindowHuh

withGL :: Maybe WindowConf -> Maybe ErrorCallback -> (Window -> IO a) -> IO a
withGL wc ec action = withErrorCallback ec . withGLFW . withNewWindow wc $ action

------------------------------------------------------------------------------
-- Util

swapBuffers :: Window -> IO ()
swapBuffers = GLFW.swapBuffers

getFramebufferSize :: Window -> IO (Int, Int)
getFramebufferSize = GLFW.getFramebufferSize

-- eof
