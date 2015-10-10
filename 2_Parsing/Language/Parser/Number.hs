module Language.Parser.Number where

import qualified Text.ParserCombinators.Parsec as P
import Control.Monad as M
import Language.Definition

parse :: P.Parser LispVal
parse = M.liftM (Number . read) $ P.many1 P.digit
--
-- 'do' notation
-- parse :: P.Parser LispVal
-- parse = do
--                 x <- P.many1 P.digit
--                 return $ Number . read $ x 
-- >>= 
-- parse :: P.Parser LispVal
-- parse =  P.many1 P.digit >>= return . Number . read
