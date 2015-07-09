{-# LANGUAGE PackageImports, GADTs #-}
-- Gpipe format to GLFW window-hint conversion
module Graphics.GPipe.Context.GLFW.Format
( toHints
) where

-- qualified
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.GPipe.Format as F

------------------------------------------------------------------------------
-- Top-level

toHints :: ContextFormat c ds -> [GLFW.WindowHint]
toHints F.ContextFormatNone = [] -- TODO: should this hint *not* to have anything? or should this be a "sane default"
toHints (F.ContextFormatDepth d) =
    [ GLFW.WindowHint'DepthBits $ F.depthBits d ]
toHints (F.ContextFormatStencil s) =
    [ GLFW.WindowHint'StencilBits $ F.stencilBits s ]
toHints (F.ContextFormatDepthStencil ds) =
    let (d, s) = F.depthStencilBits ds in
    [ GLFW.WindowHint'DepthBits d
    , GLFW.WindowHint'StencilBits s ]
toHints (F.ContextFormatColor c) =
    [ GLFW.WindowHint'RedBits $ F.redBits c
    , GLFW.WindowHint'GreenBits $ F.greenBits c
    , GLFW.WindowHint'BlueBits $ F.blueBits c ]
toHints (F.ContextFormatColorDepth c d) =
    toHints (F.ContextFormatColor c) ++
    toHints (F.ContextFormatDepth d)
toHints (F.ContextFormatColorStencil c s) =
    toHints (F.ContextFormatColor c) ++
    toHints (F.ContextFormatStencil s)
toHints (F.ContextFormatColorDepthStencil c ds) =
    toHints (F.ContextFormatColor c) ++
    toHints (F.ContextFormatDepthStencil ds)

-- eof
