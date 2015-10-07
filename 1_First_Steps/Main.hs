module Main where
import System.Environment
import Text.Read (readMaybe)

main :: IO ()
main = do
  putStr ("Enter name one: ")
  one <- getLine
  putStr ("Enter name two: ")
  two <- getLine
  let Just oneInt = readMaybe one :: Maybe Int
      Just twoInt = readMaybe two :: Maybe Int
  putStrLn ( "Hello, " ++ show (oneInt + twoInt))
