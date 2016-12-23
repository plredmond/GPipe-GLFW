module Test.Input where

import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Graphics.GPipe.Context.GLFW.Input as Input
import qualified Test.Common as C
import Text.Printf (printf)

-- Input Test

test :: Int -> IO ()
test frameCount = do
    putStrLn "== Input Test"
    putStrLn "\tListen for button inputs while rendering a scene to a window."
    runContextT (GLFW.simpleContext "Title") (ContextFormatColorDepth RGB8 Depth16) $ do
        Input.setCursorPosCallback $ Just (printf "Cursor pos: %fx%f\n" :: Double -> Double -> IO ())
        Input.setMouseButtonCallback $ Just (\b s m -> printf "Mouse: %s %s\n" (show b) (show s))
        Input.setKeyCallback $ Just (\k i s m -> printf "Key: %s %s %s\n" (show k) (show i) (show s))
        resources <- C.initRenderContext [C.plane]
        C.mainloop (frameCount, 0) resources
