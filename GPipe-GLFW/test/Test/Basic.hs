module Test.Basic where

import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Test.Common as C

-- Basic Test

test :: Int -> IO ()
test frameCount = do
    putStrLn "== Basic Test"
    putStrLn "\tRender a scene to a window."
    runContextT GLFW.newContext (ContextFormatColorDepth RGB8 Depth16) $ do
        resources <- C.initRenderContext [C.xAxis, C.yAxis, C.zAxis, C.plane]
        C.mainloop (frameCount, 0) resources

-- eof
