{-# LANGUAGE GADTs #-} -- To pattern match on 'ContextFormat' constructors.
{-# LANGUAGE DeriveAnyClass #-} -- To derive 'Exception' w/o a standalone declaration.
{-# LANGUAGE FlexibleInstances #-} -- To derive 'Exception [WindowHint]'.
module Graphics.GPipe.Context.GLFW.Format
( UnsafeWindowHintsException(..)
, allowedHint
, toHints
) where

-- stdlib
import Control.Exception (Exception)

-- third party
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.GPipe as GPipe
import Graphics.UI.GLFW (WindowHint(..))

data UnsafeWindowHintsException = UnsafeWindowHintsException [WindowHint]
    deriving
    ( Exception
    , Show
    )
instance Exception [WindowHint]

allowedHint :: WindowHint -> Bool
allowedHint (WindowHint'sRGBCapable _) = False
allowedHint (WindowHint'Visible _) = False
allowedHint (WindowHint'RedBits _) = False
allowedHint (WindowHint'GreenBits _) = False
allowedHint (WindowHint'BlueBits _) = False
allowedHint (WindowHint'AlphaBits _) = False
allowedHint (WindowHint'DepthBits _) = False
allowedHint (WindowHint'StencilBits _) = False
allowedHint (WindowHint'ContextVersionMajor _) = False
allowedHint (WindowHint'ContextVersionMinor _) = False
allowedHint (WindowHint'OpenGLForwardCompat _) = False
allowedHint (WindowHint'OpenGLProfile _) = False
allowedHint _ = True

toHints :: GPipe.ContextFormat c ds -> [GLFW.WindowHint]
toHints fmt =
    [ GLFW.WindowHint'sRGBCapable sRGB
    , GLFW.WindowHint'Visible visible
    , GLFW.WindowHint'RedBits red
    , GLFW.WindowHint'GreenBits green
    , GLFW.WindowHint'BlueBits blue
    , GLFW.WindowHint'AlphaBits alpha
    , GLFW.WindowHint'DepthBits depth
    , GLFW.WindowHint'StencilBits stencil

    , GLFW.WindowHint'ContextVersionMajor 3
    , GLFW.WindowHint'ContextVersionMinor 3
    , GLFW.WindowHint'OpenGLForwardCompat True
    , GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
    ]
    where
        ((red, green, blue, alpha, sRGB), depth, stencil) = GPipe.contextBits fmt
        visible = case fmt of
            GPipe.ContextFormatNone -> False
            _ -> True
