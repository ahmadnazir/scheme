import System.Environment
import Language.Expression as P

-- dont' know why [x:_] <- getArgs doesn't work, since the type of the list is []
main :: IO ()
main = do
  (x:_) <- getArgs 
  putStrLn ( "Hello, " ++ P.readExpr x)
