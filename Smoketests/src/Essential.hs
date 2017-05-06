import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Test.Common as C
import qualified Test.Control as A
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)

main = do
    putStrLn "== Essential wrapped interfaces"
    putStrLn $ "\tRender a scene using additional essential GLFW interfaces."
    -- TODO: make it a fullscreen window
    -- TODO: set an error callback
    -- TODO: Set some window hints
    runContextT handleConfig $ do
        win <- newWindow (WindowFormatColorDepth RGB8 Depth16) (GLFW.defaultWindowConfig "Essential")
        resources <- C.initRenderContext win [C.xAxis, C.yAxis, C.zAxis]
        C.mainloop win (A.repeat $ A.seconds 1.0) resources $ \controller -> do
            Just t <- liftIO $ GLFW.getTime
            when (t > 2.0) $ do
                Just () <- GLFW.setWindowShouldClose win True
                return ()
            shouldClose <- GLFW.windowShouldClose win
            return $ maybe False id shouldClose
    where
        handleConfig = GLFW.defaultHandleConfig {GLFW.configErrorCallback=curry print :: GLFW.ErrorCallback}
