{-# LANGUAGE Rank2Types #-} -- For `forall` in projectLines
{-# LANGUAGE FlexibleInstances #-} -- For Controller instance on a tuple
module Test.Common where

import Control.Concurrent (threadDelay)
import GHC.Float (double2Float)
import Control.Monad.Exception (MonadException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Traversable (forM)

import Graphics.GPipe.Context.GLFW.Input (getTime)
import Graphics.GPipe

xAxis :: [(V3 Float, V3 Float)]
xAxis = zip (repeat $ V3 1 0 0) -- red
    [V3 (-1) 0 0, V3 1 0 0, V3 0.8 (-0.1) 0, V3 0.8 0 0, V3 1 (-0.1) 0]

yAxis :: [(V3 Float, V3 Float)]
yAxis = zip (repeat $ V3 0 1 0) -- green
    [V3 0 (-1) 0, V3 0 1 0, V3 0 0.8 (-0.1), V3 0 0.9 (-0.05), V3 0 1 (-0.1)]

zAxis :: [(V3 Float, V3 Float)]
zAxis = zip (repeat $ V3 0 0 1) -- blue
    [V3 0 0 (-1), V3 0 0 1, V3 (-0.1) 0 0.8, V3 (-0.1) 0 1]

plane :: [(V3 Float, V3 Float)]
plane = zip (repeat $ V3 1 1 1) -- white
    [V3 (-2) (-0.1) (-2), V3 (-2) (-0.1) 2, V3 2 (-0.1) 2, V3 2 (-0.1) (-2), V3 (-2) (-0.1) (-2)]

-- TODO: make tests
--   * basic
--   * split-thread load/render
--   * render multiple windows

data ShaderEnv os = ShaderEnv
    { extractProjU :: (Buffer os (Uniform (V4 (B4 Float))), Int)
    , extractLinePA :: PrimitiveArray Lines (B3 Float, B3 Float)
    , extractRastOpts :: (Side, ViewPort, DepthRange)
    }

initBuffers
    :: (MonadIO m, ContextHandler w)
    => [[(V3 Float, V3 Float)]]
    -> Window os c ds
    -> ContextT w os m ([Buffer os (B3 Float, B3 Float)], Buffer os (Uniform (V4 (B4 Float))))
initBuffers rawMeshes win = do
    -- make mesh buffers
    meshesB <- forM rawMeshes $ \pts -> do
        buf <- newBuffer $ length pts
        writeBuffer buf 0 pts
        return buf
    -- make projection matrix buffer
    projMatB <- newBuffer 1
    return (meshesB, projMatB)

projectLines :: forall os f. Shader os  (ShaderEnv os) (FragmentStream (V3 FFloat, FFloat))
projectLines = do
    projMat <- getUniform extractProjU
    linePS <- toPrimitiveStream extractLinePA
    -- project points
    let projectedLinePS = (\(c, p) -> (projMat !* point p, c)) <$> linePS
    lineFS <- rasterize extractRastOpts projectedLinePS
    -- write fragment depths and return frags
    return $ withRasterizedInfo (\fr inf -> (fr, depth inf)) lineFS
    where
        depth RasterizedInfo {rasterizedFragCoord = (V4 _ _ z _)} = z

-- TODO: just adds the draw call to the end of the shader & glues some init functions together
initRenderContext
    :: (MonadIO m, MonadException m)
    => [[(V3 Float, V3 Float)]]
    -> Window os c ds
    -> ContextT w os m
        ( [Buffer os (B3 Float, B3 Float)]
        , Buffer os (Uniform (V4 (B4 Float)))
        , CompiledShader os (ShaderEnv os)
        )
initRenderContext rawMeshes win = do
    projShader <- compileShader (projectLines >>= drawWindowColorDepth (const (win, ContextColorOption NoBlending $ pure True, DepthOption Less True)))
    (meshesB, projMatB) <- initBuffers rawMeshes win
    return (meshesB, projMatB, projShader)

computeProjMat :: Float -> V2 Int -> M44 Float
computeProjMat frac (V2 w h) = camera2clip !*! world2camera !*! model2world
    where
        t = frac * 2 * pi
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

basicRenderer
    :: V2 Int
    -> ( [Buffer os (B3 Float, B3 Float)]
       , Buffer os (Uniform (V4 (B4 Float)))
       , CompiledShader os (ShaderEnv os)
       )
    -> Render os ()
basicRenderer size (meshesB, projMatB, projShader) = do
    clearContextColor 0.2 -- grey
    clearContextDepth 1 -- far plane
    meshPAs <- forM meshesB $ \mesh -> do
        meshVA <- newVertexArray mesh
        return $ toPrimitiveArray LineStrip meshVA
    projShader $ ShaderEnv (projMatB, 0) (mconcat meshPAs) (Front, ViewPort 0 size, DepthRange 0 1)

-- | Functions to control animation progress and completion. First argument is the result of GLFW.getTime.
class Controller c where
    done :: Double -> c -> Bool -- | Are we done yet?
    done now c = frac now c >= 1.0
    frac :: Double -> c -> Double -- | What fraction of completion have we reached?
    next :: Double -> c -> c -- | Advance to the next frame.

-- | Render a fixed set of frames, irrespective of time.
instance Controller (Int, Int) where
--  done _ (maxFrame, curFrame) = curFrame >= maxFrame
    frac _ (maxFrame, curFrame) = fromIntegral curFrame / fromIntegral maxFrame
    next _ = fmap (+1)

-- | Render for a time period, dropping missed frames.
instance Controller Double where
--  done now end = now >= end
    frac now end = now / end
    next _ end = end

mainloop
    ::  (MonadIO m, MonadException m, Controller t)
    => t
    -> ( [Buffer os (B3 Float, B3 Float)]
       , Buffer os (Uniform (V4 (B4 Float)))
       , CompiledShader os (ShaderEnv os)
       )
    -> ContextT w os m ()
mainloop frame resources@(_, projMatB, _) = do
    t <- liftIO $ getTime
    case t of
        Nothing -> (liftIO $ putStrLn "Warning: unable to getTime") >> mainloop frame resources
        Just now | done now frame -> return ()
                 | otherwise -> do
            -- compute a projection matrix & write it to the buffer
            size <- getContextBuffersSize
            writeBuffer projMatB 0 [computeProjMat (double2Float $ frac now frame) size]
            -- render the scene and then loop
            render $ basicRenderer size resources
            swapContextBuffers
            mainloop (next now frame) resources
