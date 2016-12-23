module Test.Multi where

import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW

test :: Int -> IO ()
test _ = do
    putStrLn "== Multi window test"
    putStrLn "\tUse shared contexts to load resources and render different subsets to different windows."
    putStrLn "!! Shared contexts aren't implemented, expect a crash."
    runContextT (GLFW.simpleContext "Test") (ContextFormatColorDepth RGB8 Depth16) $ do
        -- TODO: in this thread render the axes
        -- TODO: fork; in other thread render just C.plane
        runSharedContextT (ContextFormatColorDepth RGB8 Depth16) $ do
            return ()
