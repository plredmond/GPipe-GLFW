module Main where

-- qualified
import qualified Text.Printf as P
import qualified Control.Concurrent as C
import qualified Control.Exception as Exc
import qualified Graphics.GPipe.Context as Ctx
import qualified Graphics.GPipe.Format as Fmt
import qualified Graphics.GPipe.Context.GLFW as CtxI

main :: IO ()
main = do
    -- create and destroy contexts in sequence
    withContext (CtxI.newContext Fmt.ContextFormatNone) (action "a")
    withContext (CtxI.newContext $ Fmt.ContextFormatColor Fmt.RGB8) (action "b")
    -- create a context
    --     create and destroy a 1st shared context
    --     create a 2nd shared context in sequence
    --         create and destroy a nested shared context from the 2nd shared context
    --     destroy the 2nd shared context
    -- destroy the original context
    let fmt = Fmt.ContextFormatColor Fmt.RG32F
    withContext (CtxI.newContext fmt) $ \c1 -> do
            action "c1 before" c1
            withContext (Ctx.newSharedContext c1 fmt) $ \c2a -> action "c1/c2a" c2a
            withContext (Ctx.newSharedContext c1 fmt) $ \c2b -> do
                    action "c1/c2b" c2b
                    withContext (Ctx.newSharedContext c2b fmt) $ \c3 -> action "c1/c2b/c3" c3
            action "c1 after" c1

-- Report a context's thread id to the console
action :: String -> Ctx.ContextHandle -> IO ()
action trace ch = do
    tid <- C.myThreadId
    ctid <- Ctx.contextDoSync ch C.myThreadId
    P.printf "[Trace: %s] %s sees a context at %s\n"
        trace (show tid) (show ctid)
    Ctx.contextDoAsync ch $ C.myThreadId >>= P.printf "[Trace: %s] <Async> %s reporting!\n" trace . show
--  C.threadDelay 1

-- Run an action in a context
withContext :: IO Ctx.ContextHandle -> (Ctx.ContextHandle -> IO a) -> IO a
withContext contextCreate =
    Exc.bracket
        contextCreate
        Ctx.contextDelete

-- eof
