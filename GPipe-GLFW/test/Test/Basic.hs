module Test.Basic where

import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Test.Common as C

test :: IO ()
test = do
    putStrLn "== Basic Test"
    putStrLn $ "\tRender a scene to a window using a configured context."
    runContextT context (ContextFormatColorDepth RGB8 Depth16) $ do
        resources <- C.initRenderContext [C.xAxis, C.yAxis, C.zAxis]
        C.mainloop (30 :: Int, 0 :: Int) resources
    where
        context = GLFW.context
            (\err msg -> putStrLn $ "GLFW Error [" ++ show err ++ "]: " ++ msg)
            (GLFW.WindowConfig 640 480 "Test" GLFW.Poll Nothing [])
