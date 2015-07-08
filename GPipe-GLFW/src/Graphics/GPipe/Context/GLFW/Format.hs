{-# LANGUAGE PackageImports #-}
-- Gpipe format to GLFW window-hint conversion
module Graphics.GPipe.Context.GLFW.Format
( toHints
, resetHints
, hinted
) where

-- qualified
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW

-- unqualified
import Graphics.GPipe.Format (ContextFormat)

------------------------------------------------------------------------------
-- Top-level

toHints :: ContextFormat c ds -> [GLFW.WindowHint]
toHints = undefined

resetHints :: [GLFW.WindowHint] -> IO ()
resetHints hints = do
    GLFW.defaultWindowHints
    mapM_ GLFW.windowHint hints

hinted :: ContextFormat c ds -> IO ()
hinted = resetHints . toHints

-- eof
