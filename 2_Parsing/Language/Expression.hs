module Language.Expression where

import qualified Text.ParserCombinators.Parsec as P hiding (spaces)
import Text.Read (readMaybe)

import Language.Definition 
import Language.Parser.Number as Number

symbol :: P.Parser Char
symbol = P.oneOf "!#$%&|*+-/:<=>?@^_~"
-- @todo: here now.. need to add support for numbers in different bases
-- symbol = P.oneOf "!$%&|*+-/:<=>?@^_~"

-- Detect spaces
spaces :: P.Parser ()
spaces = P.skipMany1 P.space

-- Detect escaped characters
escapedChars :: P.Parser Char
escapedChars = do P.char '\\'
                  x <- P.oneOf ['\\', '"', 't', 'n']
                  return $ case x of
                             't' -> '\t'
                             'n' -> '\n'
                             _   -> x

parseBool :: P.Parser LispVal
parseBool = do
    P.char '#'
    (P.char 't' >> return (Bool True))  P.<|> (P.char 'f' >> return (Bool False))


parseString :: P.Parser LispVal
parseString = do
                P.char '"'
                x <- P.many( escapedChars P.<|> P.noneOf "\"") -- why does the order matter?
                P.char '"'
                return $ String x

-- The true and false checks need to be changed
-- parseAtom :: P.Parser LispVal
-- parseAtom = do
--               first <- P.letter P.<|> symbol
--               rest <- P.many (P.letter P.<|> P.digit P.<|> symbol)
--               let atom = first:rest
--               return $ case atom of
--                          "#t" -> Bool True
--                          "#f" -> Bool False
--                          _    -> Atom atom



parseExpr :: P.Parser LispVal
parseExpr = (parseString P.<|> Number.parse P.<|> parseBool)

-- @todo: maybe this can go somewhere else
readExpr :: String -> String
readExpr input = case P.parse parseExpr "lisp" input of
              Left error -> "Error: " ++ show error
              Right val  -> "Found val" ++ show val 

