module Graphics.GPipe.Context.GLFW
( -- * Creating contexts
  simpleContext
, context
  -- * Data types
, WrappedWindow()
, WindowConfig(..)
, EventPolicy(..)
, UnsafeWindowHintsException(..)
) where

-- stdlib
import Text.Printf (printf)
import Control.Monad (unless)
import Control.Exception (throwIO)
import Control.Concurrent
    ( newEmptyMVar, takeMVar, putMVar
    , Chan, newChan, readChan, writeChan
    , myThreadId
    , threadDelay
    )
import Control.Concurrent.Async
    ( Async
    , withAsync, withAsyncBound
    , wait, poll
    )

-- third party
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GPipe.Context (ContextHandle(..), ContextManager)

-- local
import qualified Graphics.GPipe.Context.GLFW.Input as Input
import qualified Graphics.GPipe.Context.GLFW.Format as Format
import Graphics.GPipe.Context.GLFW.Format (UnsafeWindowHintsException(..))
import qualified Graphics.GPipe.Context.GLFW.Resource as Resource
import Graphics.GPipe.Context.GLFW.Resource (WrappedWindow(), WindowConfig(..), EventPolicy(..))

-- update gpipe-glfw:
--  - reexport Input module and check how it renders in haddocks
--  - make it monad generic (currently tied to IO, and that doesn't work with shared contexts)
--  - find a way to take config args for shared contexts

data RPC
    = Execute (IO ())
    | Shutdown
    | Noop

pp :: String -> IO ()
pp msg = do
    t <- Input.getTime
    tid <- myThreadId
    c <- GLFW.getCurrentContext

    printf "[%s %s, %s]: %s\n" (show t) (show tid) (show c) msg

-- | A context for a `640`x`480` window with the given `title` which polls for events after every frame. Logs errors.
simpleContext :: String -> ContextManager c ds WrappedWindow IO a
simpleContext title = context
    (\err msg -> printf "GLFW Error [%s]: %s\n" (show err) msg)
    (WindowConfig 640 480 title Poll Nothing [])

-- | A fully configurable Context.
context :: GLFW.ErrorCallback -> WindowConfig -> ContextManager c ds WrappedWindow IO a
context errorCallback windowConfig format action = do
    pp "main says hello"
    comm <- newChan
    runFork comm (Left errorCallback) windowConfig format action

runFork :: Chan RPC -> Either GLFW.ErrorCallback GLFW.Window -> WindowConfig -> ContextManager c ds WrappedWindow IO a
runFork comm variety windowConfig format action = do
    unless (null badHints) $
        throwIO (UnsafeWindowHintsException badHints)
    withAsyncBound child whileAsync
    where
        badHints = filter (not . Format.allowedHint) $ wc'hints windowConfig

        -- Thing to do while the child-thread runs the given action.
        whileAsync = case variety of
            Left _ -> mainloop comm -- The main-thread remains busy throughout the life of the program.
            Right _ -> wait -- Shared contexts achieve no paralellism.

        -- Method of creating the OpenGL context.
        withCtx = case variety of
            Left errorCallback -> Resource.withContext inject errorCallback -- The first context associates an error callback.
            Right parent -> Resource.withSharedContext inject parent -- Shared contexts are created from a parent context.

        -- Way to wrap up after the child then after the context is destroyed.
        afterCtx = case variety of
            Left _ -> pp "topmost child after context is destroyed" >> writeChan comm Shutdown -- Tell the main-thread to wrap up.
            Right _ -> pp "child after context is destroyed"

        -- Thread to which the OpenGL context is bound and where GPipe runs.
        child = do
            pp "child says hi"
            result <- withCtx windowConfig {wc'hints = Format.toHints format ++ wc'hints windowConfig}
                (\window -> do
                    GLFW.makeContextCurrent $ Just window
                    pp "child got a context"
                    action ContextHandle
                        { withSharedContext = \_ _ -> error "shared contexts are not implemented" -- runFork comm (Right window) (WindowConfig 640 480 "Another Window" Wait Nothing [])
                        -- FIXME: ^ doesn't compile because we assume (m ~ IO) in this file and we get no (MonadIO m) etc from the signature.
                        , contextDoSync = (\_ a -> a)
                        , contextDoAsync = (\_ a -> a >> return ())
                        , contextSwap = GLFW.swapBuffers window >> processEvents
                        , contextFrameBufferSize = GLFW.getFramebufferSize window
                        , contextWindow = Resource.WrappedWindow window
                        }
                )
            afterCtx
            return result

        -- Function to run after swapBuffers
        processEvents = case wc'eventPolicy windowConfig of
            Poll -> GLFW.pollEvents
            Wait -> GLFW.waitEvents

        -- Run the context creation action on the main-thread via RPC
        inject create = do
            var <- newEmptyMVar
            writeChan comm $ Execute (pp "somebody is creating" >> create >>= putMVar var >> pp "somebody's creation is complete")
            result <- takeMVar var
            return result

-- RPC receiver to be run on the main-thread
mainloop :: Chan RPC -> Async b -> IO b
mainloop comm child = do
    -- Wake up after 1 second w/o rpc
    rpc <- withAsync (threadDelay (round (1e6 :: Float)) >> writeChan comm Noop)
        (const $ readChan comm)
    case rpc of
        Execute action -> pp "main got an rpc" >> action >> mainloop comm child
        Shutdown -> pp "main waits for last child to exit" >> wait child
        Noop -> do
            status <- poll child
            case status of
                Nothing -> mainloop comm child
                Just _ -> pp "main saw child has quit and ditto" >> wait child
