module Language.Parser.Expression where

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

bool :: P.Parser LispVal
bool = do
    P.char '#'
    (P.char 't' >> return (Bool True))  P.<|> (P.char 'f' >> return (Bool False))

-- @fixme: get rid of try
-- @todo: there are other types of characters as well
char :: P.Parser LispVal
char  = do P.string "#\\"
           x <- (
                  P.try (P.string "space")   P.<|> 
                  P.try (P.string "newline") P.<|> 
                  do c <- P.anyChar
                     P.notFollowedBy P.alphaNum
                     return [c]
                )
           return $ Char $ case x of
                    "space"   -> ' '
                    "newline" -> '\n'
                    _         -> x !! 0

string :: P.Parser LispVal
string = do
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

-- List
-- list :: Parser LispVal
-- list = liftM List $ sepBy parseExpr spaces


-- @fixme: get rid of try
expression :: P.Parser LispVal
expression = (string P.<|> P.try Number.parse P.<|> P.try bool P.<|> char)


