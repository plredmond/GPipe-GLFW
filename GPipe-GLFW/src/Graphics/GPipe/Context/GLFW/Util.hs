{-# LANGUAGE PackageImports #-}
-- Pass-through functions
module Graphics.GPipe.Context.GLFW.Util
( swapBuffers
, getFramebufferSize
) where

-- qualified
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW

------------------------------------------------------------------------------
-- Util

swapBuffers :: GLFW.Window -> IO ()
swapBuffers = GLFW.swapBuffers

getFramebufferSize :: GLFW.Window -> IO (Int, Int)
getFramebufferSize = GLFW.getFramebufferSize

-- eof
