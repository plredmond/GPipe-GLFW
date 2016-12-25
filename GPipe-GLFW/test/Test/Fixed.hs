module Test.Fixed where

import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Test.Common as C

test :: IO ()
test = do
    putStrLn "== Fixed Test"
    putStrLn $ "\tRender a scene to a window for " ++ show maxFrames ++ " frames."
    runContextT (GLFW.simpleContext "Test") (ContextFormatColorDepth RGB8 Depth16) $ do
        resources <- C.initRenderContext [C.xAxis, C.yAxis, C.zAxis, C.plane]
        C.mainloop (maxFrames, 0 :: Int) resources
    where
        maxFrames = 60 :: Int
