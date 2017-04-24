import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Test.Common as C

main :: IO ()
main = do
    putStrLn "== Fixed frame count"
    putStrLn $ "\tRender a scene to a window for " ++ show maxFrames ++ " frames."
    runContextT GLFW.defaultHandleConfig $ do
        win <- newWindow (WindowFormatColorDepth RGB8 Depth16) (GLFW.defaultWindowConfig "Fixed")
        resources <- C.initRenderContext win [C.xAxis, C.yAxis, C.zAxis, C.plane]
        C.mainloop win (maxFrames, 0 :: Int) resources
    where
        maxFrames = 60 :: Int
