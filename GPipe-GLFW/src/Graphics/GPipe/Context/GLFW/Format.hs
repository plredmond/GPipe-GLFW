{-# LANGUAGE PackageImports, GADTs #-}
-- Gpipe format to GLFW window-hint conversion
module Graphics.GPipe.Context.GLFW.Format
( toHints
) where

-- qualified
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import qualified Graphics.GPipe.Format as F

------------------------------------------------------------------------------
-- Top-level

toHints :: F.ContextFormat c ds -> [GLFW.WindowHint]
toHints fmt =
    [ GLFW.WindowHint'sRGBCapable sRGB
    , GLFW.WindowHint'Visible visible
    , GLFW.WindowHint'RedBits red
    , GLFW.WindowHint'GreenBits green
    , GLFW.WindowHint'BlueBits blue
    , GLFW.WindowHint'AlphaBits alpha
    , GLFW.WindowHint'DepthBits depth
    , GLFW.WindowHint'StencilBits stencil
    ]
    where
        ((red, green, blue, alpha, sRGB), depth, stencil) = F.contextBits fmt
        visible = case fmt of
            F.ContextFormatNone -> False
            _ -> True

-- eof
