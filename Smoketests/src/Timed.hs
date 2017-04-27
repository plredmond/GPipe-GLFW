import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Test.Common as C

main :: IO ()
main = do
    putStrLn "== Timed render"
    putStrLn $ "\tRender a scene to a window for " ++ show maxSeconds ++ " seconds."
    runContextT GLFW.defaultHandleConfig $ do
        win <- newWindow (WindowFormatColorDepth RGB8 Depth16) (GLFW.defaultWindowConfig "Timed")
        resources <- C.initRenderContext win [C.xAxis, C.yAxis, C.zAxis, C.plane]
        C.mainloop win maxSeconds resources (return ())
    where
        maxSeconds = 2.0 :: Double
