module Language.Parser.Number where

import qualified Text.ParserCombinators.Parsec as P
-- import Control.Monad as M
import Language.Definition
import qualified Language.Number as N

parse :: P.Parser LispVal
parse = decimal1 P.<|> (P.char '#' >> (decimal2 P.<|> hex P.<|> oct P.<|> bin))
-- simple implementation
-- parse = M.liftM (Number . read) $ P.many1 P.digit
--
-- 'do' notation
-- parse :: P.Parser LispVal
-- parse = do
--                 x <- P.many1 P.digit
--                 return $ Number . read $ x 
-- >>= 
-- parse :: P.Parser LispVal
-- parse =  P.many1 P.digit >>= return . Number . read

decimal1 :: P.Parser LispVal
decimal1 = P.many1 P.digit >>= (return . Number . read)

decimal2 :: P.Parser LispVal
-- decimal2 = do try $
decimal2 = do P.string "d"
              x <- P.many1 P.digit
              (return . Number . read) $ x

hex :: P.Parser LispVal
-- hex = do try $ string "#x"
hex = do P.string "x"
         x <- P.many1 P.hexDigit
         return $ Number (N.hex2dec x)

oct :: P.Parser LispVal
-- hex = do try $ string "#x"
oct = do P.string "o"
         x <- P.many1 P.octDigit
         return $ Number (N.oct2dec x)

bin :: P.Parser LispVal
-- hex = do try $ string "#x"
bin = do P.string "b"
         x <- P.many1 (P.oneOf "10")
         return $ Number (N.bin2dec x)
