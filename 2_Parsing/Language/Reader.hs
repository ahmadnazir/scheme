module Language.Reader where

import qualified Text.ParserCombinators.Parsec as P hiding (spaces)

import qualified Language.Parser.Expression as EP
import qualified Language.Parser.List as EL

import qualified Text.ParserCombinators.Parsec as P

-- @todo: maybe this can go somewhere else
read :: String -> String
read input = case P.parse EP.expression "lisp" input of
             Left error -> "Error: " ++ show error
             Right val  -> "Found val" ++ show val

