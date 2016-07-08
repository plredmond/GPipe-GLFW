import qualified Test.Basic as Basic
import qualified Test.Split as Split
import qualified Test.Multi as Multi

main :: IO ()
main = do
    putStrLn "\n= Running smoke tests"
    Basic.test (3 * 60)
    Split.test (3 * 60)
    Multi.test (3 * 60)
