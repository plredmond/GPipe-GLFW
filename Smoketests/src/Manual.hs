import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Test.Common as C

main :: IO ()
main = do
    putStrLn "== Manual event processing"
    putStrLn $ "\tRender a scene to a window without automatic event processing."
    runContextT handleConfig $ do
        win <- newWindow (WindowFormatColorDepth RGB8 Depth16) (GLFW.defaultWindowConfig "Manual")
        resources <- C.initRenderContext win [C.xAxis, C.yAxis, C.zAxis]
        C.mainloop win (60 :: Int, 0 :: Int) resources (GLFW.mainstep win GLFW.Poll)
    where
        -- Config which disables automatic event processing
        handleConfig = GLFW.defaultHandleConfig {GLFW.configEventPolicy=Nothing}
