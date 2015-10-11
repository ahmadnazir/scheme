module Language.Parser.List where

import qualified Language.Definition as D
import qualified Language.Parser.Expression as EP
import Control.Monad as M

import qualified Text.ParserCombinators.Parsec as P hiding (spaces)

list :: P.Parser D.LispVal
list = M.liftM D.List $ P.sepBy EP.expression EP.spaces

dottedList :: P.Parser D.LispVal
dottedList = do
  head <- P.endBy EP.expression EP.spaces
  tail <- P.char '.' >> EP.spaces >> EP.expression
  return $ D.DottedList head tail
