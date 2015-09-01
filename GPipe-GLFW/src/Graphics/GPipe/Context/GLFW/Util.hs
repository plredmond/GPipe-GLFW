{-# LANGUAGE PackageImports #-}
-- Pass-through functions
module Graphics.GPipe.Context.GLFW.Util
( swapBuffers
, getFramebufferSize
) where

import qualified "GLFW-b" Graphics.UI.GLFW as GLFW

------------------------------------------------------------------------------
-- Util

swapBuffers :: GLFW.Window -> IO ()
swapBuffers w = GLFW.makeContextCurrent (Just w) >> GLFW.swapBuffers w >> GLFW.pollEvents

getFramebufferSize :: GLFW.Window -> IO (Int, Int)
getFramebufferSize w = GLFW.makeContextCurrent (Just w) >> GLFW.getFramebufferSize w

-- eof
