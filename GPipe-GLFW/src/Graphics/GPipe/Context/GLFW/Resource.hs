-- | Internal module defining resources and associated types
module Graphics.GPipe.Context.GLFW.Resource where

-- stdlib
import Text.Printf (printf)
-- thirdparty
import qualified Graphics.UI.GLFW as GLFW
    ( Monitor
    , WindowHint
    , ErrorCallback
    )

-- | Configuration for the GLFW library.
data HandleConfig = HandleConfig
    { configErrorCallback :: GLFW.ErrorCallback
    }

-- | Configuration for a new GLFW window and associated OpenGL context.
data WindowConfig = WindowConfig
    { configWidth :: Int
    , configHeight :: Int
    , configTitle :: String
    , configMonitor :: Maybe GLFW.Monitor
    , configHints :: [GLFW.WindowHint]
    , configSwapInterval :: Maybe Int
    } deriving
    ( Show
    )

-- | Default configuration which prints any errors that GLFW emits.
defaultHandleConfig :: HandleConfig
defaultHandleConfig = HandleConfig errorHandler
    where
        -- TODO: swap printf for logger
        -- TODO: accumulate channel errors in a list or channel in handle until they can be processed somewhere
        errorHandler err desc = printf "%s: %s\n" (show err) desc

-- | Default window configuration for a small window on any monitor with the given title.
defaultWindowConfig :: String -> WindowConfig
defaultWindowConfig title = WindowConfig 640 480 title Nothing [] Nothing
