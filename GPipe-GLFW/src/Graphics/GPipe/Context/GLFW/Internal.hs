{-# LANGUAGE PackageImports #-}
module Graphics.GPipe.Context.GLFW.Internal where

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
defaultOnError :: GLFW.ErrorCallback
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
withErrorCallback :: Maybe GLFW.ErrorCallback -> IO a -> IO a
withErrorCallback customOnError =
    GLFW.setErrorCallback (Just $ M.fromMaybe defaultOnError customOnError)
    `Exc.bracket_`
    GLFW.setErrorCallback Nothing 

-- init and terminate GLFW
withGLFW :: IO a -> IO a
withGLFW = GLFW.init `Exc.bracket_` GLFW.terminate

-- create and destroy a new window, as the current context, using any monitor
withNewWindow :: Maybe WindowConf -> (GLFW.Window -> IO a) -> IO a
withNewWindow customWindowConf =
    (M.fromMaybe noWindow <$> createWindowHuh)
    `Exc.bracket`
    GLFW.destroyWindow
    where
        noWindow = error "Couldn't create a window"
        WindowConf w h t = M.fromMaybe defaultWindowConf customWindowConf
        createWindowHuh = do
            win <- GLFW.createWindow w h t Nothing Nothing
            GLFW.makeContextCurrent win
            return win

withGL :: Maybe WindowConf -> Maybe GLFW.ErrorCallback -> (GLFW.Window -> IO a) -> IO a
withGL wc ec action = withErrorCallback ec . withGLFW . withNewWindow wc $ action

-- eof
