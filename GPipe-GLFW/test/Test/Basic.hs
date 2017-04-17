module Test.Basic where

import Data.Proxy
import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Test.Common as C

test :: IO ()
test = do
    putStrLn "== Basic Test"
    putStrLn $ "\tRender a scene to a window using a configured context."
    runContextT GLFW.defaultHandleConfig $ do
        win <- GLFW.newWindow (ContextFormatColorDepth RGB8 Depth16) (ContextFormatColorDepth RGB8 Depth16)
        resources <- C.initRenderContext [C.xAxis, C.yAxis, C.zAxis]
        C.mainloop (30 :: Int, 0 :: Int) resources
