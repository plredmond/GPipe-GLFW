-- | Internal module for generating and assessing GLFW hint lists
module Graphics.GPipe.Context.GLFW.Format where

-- stdlib
import Control.Exception (Exception)
-- third party
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.GPipe as GPipe
import Graphics.UI.GLFW (WindowHint(..))

-- | IO Exception thrown when attempting to create a new window using GLFW
-- hints which GPipe manages.
data UnsafeWindowHintsException
    = UnsafeWindowHintsException [WindowHint]
    deriving Show
instance Exception UnsafeWindowHintsException

data OpenGlVersion = OpenGlVersion
    { openGlVersionMajor :: Int
    , openGlVersionMinor :: Int
    }

allowedHint :: WindowHint -> Bool
allowedHint (WindowHint'Visible _) = False
allowedHint (WindowHint'sRGBCapable _) = False
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

versionToHints :: Int -> Int -> [GLFW.WindowHint]
versionToHints major minor =
    [ WindowHint'ContextVersionMajor major
    , WindowHint'ContextVersionMinor minor
    , WindowHint'OpenGLForwardCompat True
    , WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
    ]

bitsToHints :: Maybe GPipe.WindowBits -> [GLFW.WindowHint]
bitsToHints Nothing = [GLFW.WindowHint'Visible False]
bitsToHints (Just ((red, green, blue, alpha, sRGB), depth, stencil)) =
    [ WindowHint'sRGBCapable sRGB
    , WindowHint'RedBits $ Just red
    , WindowHint'GreenBits $ Just green
    , WindowHint'BlueBits $ Just blue
    , WindowHint'AlphaBits $ Just alpha
    , WindowHint'DepthBits $ Just depth
    , WindowHint'StencilBits $ Just stencil
    ]
