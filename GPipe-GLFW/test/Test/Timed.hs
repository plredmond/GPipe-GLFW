module Test.Timed where

import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Test.Common as C

test :: IO ()
test = do
    putStrLn "== Timed Test"
    putStrLn $ "\tRender a scene to a window for " ++ show maxSeconds ++ " seconds."
    runContextT (GLFW.simpleContext "Test") (ContextFormatColorDepth RGB8 Depth16) $ do
        resources <- C.initRenderContext [C.xAxis, C.yAxis, C.zAxis, C.plane]
        C.mainloop maxSeconds resources
    where
        maxSeconds = 2.0 :: Double
