-- | Exposes some underlying implementation details which can be used to
-- gain access to 'Graphics.UI.GLFW' functionality that isn't exposed by this
-- library otherwise, but which can be dangerous if used incorrectly.
--
-- The underlying 'Graphics.UI.GLFW.Window' object can be retrieved from a
-- 'GLFWWindow' using 'getGLFWWindow'.It can be used inside a
-- 'Graphics.GPipe.Context.ContextT' as follows:
--
-- > withContextWindow (\win -> doSomething (getGLFWWindow win))
--
-- Bear in mind that it is possible to do bad things with this. For example, using
-- 'GLFW.makeContextCurrent' could cause GPipe to lose control of the window,
-- and 'GLFW.destroyWindow' is bad for obvious reasons.
--
-- See 'Graphics.GPipe.Context.GLFW.Input' for concrete examples.
module Graphics.GPipe.Context.GLFW.Unsafe
       ( GLFWWindow(..) )
       where

import Graphics.GPipe.Context.GLFW.Resource (GLFWWindow(..))
