import qualified System.Environment as Env
import qualified Test.Basic as Basic
import qualified Test.Input as Input
import qualified Test.Split as Split
import qualified Test.Multi as Multi

-- TODO: test with and without mesa support and observe errors
main :: IO ()
main = do
    putStrLn "\n= Running smoke tests"
    Env.lookupEnv "LIBGL_ALWAYS_SOFTWARE" >>= \val -> do
        putStrLn $ "LIBGL_ALWAYS_SOFTWARE: " ++ show val
        case val of
            Nothing -> putStrLn "!! If you don't have hardware support, expect a crash."
            _ -> return ()
    Basic.test (3 * 60)
    Input.test (3 * 60)
    Split.test (3 * 60)
    Multi.test (3 * 60)
