import qualified System.Environment as Env
import qualified Test.Basic as Basic
import qualified Test.Split as Split
import qualified Test.Multi as Multi

main :: IO ()
main = do
    putStrLn "\n= Running smoke tests"
    Env.lookupEnv "LIBGL_ALWAYS_SOFTWARE" >>= \val -> putStrLn $ "LIBGL_ALWAYS_SOFTWARE: " ++ show val
    Basic.test (3 * 60)
    Split.test (3 * 60)
    Multi.test (3 * 60)
