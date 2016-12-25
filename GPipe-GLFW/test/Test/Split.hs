module Test.Split where

import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW

test :: IO ()
test = do
    putStrLn "== Split thread test"
    putStrLn "\tUse shared contexts to load resources on one thread and render on another."
    putStrLn "!! Shared contexts aren't implemented, expect a crash."
    runContextT (GLFW.simpleContext "Test") (ContextFormatColorDepth RGB8 Depth16) $ do
        -- TODO: in main thread, make buffers but leave them empty
        -- TODO: fork; in other thread change contents of buffers once a second
        runSharedContextT (ContextFormatNone) $ do
            return ()
