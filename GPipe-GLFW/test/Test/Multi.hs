module Test.Multi where

import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Test.Common as C

test :: Int -> IO ()
test _ = do
    putStrLn "== Multi window test"
    putStrLn "\tUse shared contexts to load resources and render different subsets to different windows."
    runContextT GLFW.newContext (ContextFormatColorDepth RGB8 Depth16) $ do
        -- TODO: in main thread render the axes

        -- TODO: fork; in other thread render just C.plane
        runSharedContextT (ContextFormatColorDepth RGB32F Depth16) $ do
            return ()
