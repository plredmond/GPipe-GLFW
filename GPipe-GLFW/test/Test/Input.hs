module Test.Input where

import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Graphics.GPipe.Context.GLFW.Input as Input
import qualified Test.Common as C
import Text.Printf (printf)

test :: IO ()
test = do
    putStrLn "== Input Test"
    putStrLn "\tListen for button inputs while rendering a scene to a window."
    runContextT (GLFW.simpleContext "Test") (ContextFormatColorDepth RGB8 Depth16) $ do
        Input.setCursorPosCallback $ Just (printf "Cursor pos: %fx%f\n" :: Double -> Double -> IO ())
        Input.setMouseButtonCallback $ Just (\b s m -> printf "Mouse: %s %s\n" (show b) (show s))
        Input.setKeyCallback $ Just (\k i s m -> printf "Key: %s %s %s\n" (show k) (show i) (show s))
        resources <- C.initRenderContext [C.plane, C.yAxis]
        C.mainloop (120 :: Int, 0 :: Int) resources
