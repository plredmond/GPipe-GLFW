--import Control.Monad.IO.Class (MonadIO)
import Control.Concurrent (threadDelay)
import Control.Monad.Trans.Class (lift)
import Data.Traversable (forM)

import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW

meshes :: [[(V3 Float, V3 Float)]]
meshes =
    [ zip (repeat $ V3 1 0 0) -- red x axis
        [V3 (-1) 0 0, V3 1 0 0, V3 0.8 (-0.1) 0, V3 0.8 0 0, V3 1 (-0.1) 0]
    , zip (repeat $ V3 0 1 0) -- green y axis
        [V3 0 (-1) 0, V3 0 1 0, V3 0 0.8 (-0.1), V3 0 0.9 (-0.05), V3 0 1 (-0.1)]
    , zip (repeat $ V3 0 0 1) -- blue z axis
        [V3 0 0 (-1), V3 0 0 1, V3 (-0.1) 0 0.8, V3 (-0.1) 0 1]
    , zip (repeat $ V3 1 1 1) -- white plane mesh
        [V3 (-2) (-0.1) (-2), V3 (-2) (-0.1) 2, V3 2 (-0.1) 2, V3 2 (-0.1) (-2), V3 (-2) (-0.1) (-2)]
    ]

main :: IO ()
main = do
    basicTest

basicTest :: IO ()
basicTest = runContextT GLFW.newContext (ContextFormatColorDepth RGB8 Depth16)
    (mainloop (3 * 60) Nothing)

data ShaderEnv = ShaderEnv
    { extractPointsPA :: PrimitiveArray Lines (B3 Float, B3 Float)
    , extractRastOpts :: (Side, ViewPort, DepthRange)
    }

mainloop :: Int -> Maybe
    ( ( [Buffer os (B3 Float, B3 Float)]
      , Buffer os (Uniform (V4 (B4 Float)))
      , CompiledShader os (ContextFormat RGBFloat Depth) ShaderEnv
      )
    , Int
    ) -> ContextT w os (ContextFormat RGBFloat Depth) IO ()
mainloop maxFrame Nothing = do -- ContextT monad transformer
    -- make mesh buffers
    meshesB <- forM meshes $ \pts -> do
        buf <- newBuffer $ length pts
        writeBuffer buf 0 pts
        return buf
    -- make projection matrix buffer
    projMatB <- newBuffer 1
    -- make simple projection shader
    projShader <- compileShader $ do -- Shader monad
        proj <- getUniform (const (projMatB, 0))
        pointsPS <- toPrimitiveStream extractPointsPA
        -- project points and then rasterize them
        lineFS <- rasterize extractRastOpts
            . fmap (\(c, p) -> (proj !* point p, c)) $ pointsPS
        -- write fragment depths and then draw them to the screen
        let depth RasterizedInfo {rasterizedFragCoord = (V4 _ _ z _)} = z
        drawContextColorDepth (const (ContextColorOption NoBlending $ pure True, DepthOption Less True))
            . withRasterizedInfo (\fr inf -> (fr, depth inf)) $ lineFS -- write fragment depth
    mainloop maxFrame $ Just ((meshesB, projMatB, projShader), 0)
mainloop maxFrame resources@(Just ((meshesB, projMatB, projShader), curFrame))
    | curFrame > maxFrame = return ()
    | otherwise = do -- ContextT monad transformer
        -- compute a projection matrix & write it to the buffer
        size@(V2 w h) <- getContextBuffersSize
        let t = fromIntegral curFrame / fromIntegral maxFrame * 2 * pi
            model2world = identity
            world2camera = lookAt
                -- eye: camera position in world coordinates
                (V3 (3 * sin t) (0.5 + cos (t * 2)) (3 * cos t))
                (V3 0 0.1 0) -- center: camera look-at target position in world coords
                (V3 0 1 0) -- up: camera up-vector
            camera2clip = perspective
                (pi / 3) -- 60deg field of view "in y direction"
                (fromIntegral w / fromIntegral h) -- aspect ratio
                1 100 -- near and far clipping plane
        writeBuffer projMatB 0 [camera2clip !*! world2camera !*! model2world]
        -- render the scene and then loop
        render $ do -- Render monad
            clearContextColor 0.2 -- grey
            clearContextDepth 1 -- far plane
            meshPAs <- forM meshesB $ \mesh -> do
                meshVA <- newVertexArray mesh
                return $ toPrimitiveArray LineStrip meshVA
            projShader $ ShaderEnv (mconcat meshPAs) (Front, ViewPort 0 size, DepthRange 0 1)
        swapContextBuffers
        lift . threadDelay . round $ 1e6 / (60 :: Float) -- fps <= 60
        mainloop maxFrame (fmap (+1) <$> resources)
