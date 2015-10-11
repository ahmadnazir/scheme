import System.Environment
import Language.Reader as R

-- dont' know why [x:_] <- getArgs doesn't work, since the type of the list is []
main :: IO ()
main = do
  (x:_) <- getArgs 
  putStrLn ( "Hello, " ++ R.read x)
