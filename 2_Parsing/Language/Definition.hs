module Language.Definition where

import qualified Data.Complex as C

-- Data Type
-- @todo: the number tower needs to be implemented
--
-- @fixme: It doesn't seem like the DottedList is implemented
-- correctly. I copied it from the tutorial
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Char Char
             | String String
             | Bool Bool
             | Number Integer
             | Float Double
             | Ratio Rational
             | Complex (C.Complex Double)
             deriving Show

