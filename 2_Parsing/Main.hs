import qualified Text.ParserCombinators.Parsec as P hiding (spaces)
import System.Environment
import Control.Monad as M
import Text.Read (readMaybe)

symbol :: P.Parser Char
symbol = P.oneOf "!#$%&|*+-/:<=>?@^_~"


-- Detect spaces
spaces :: P.Parser ()
spaces = P.skipMany1 P.space

readExp :: String -> String
readExp input = case P.parse parseExpr "lisp" input of
        Left error -> "Error: " ++ show error
        Right val  -> "Found val"

-- Data Type
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseString :: P.Parser LispVal
parseString = do
                P.char '"'
                x <- P.many (P.noneOf "\"")
                P.char '"'
                return $ String x

parseAtom :: P.Parser LispVal
parseAtom = do 
              first <- P.letter P.<|> symbol
              rest <- P.many (P.letter P.<|> P.digit P.<|> symbol)
              let atom = first:rest
              return $ case atom of 
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

-- parseNumber :: P.Parser LispVal
-- parseNumber = M.liftM (Number . read) $ P.many1 P.digit
--
-- Implementing parse number with the do notation
parseNumber :: P.Parser LispVal
parseNumber = do
                x <- P.many1 P.digit
                let Just y = readMaybe x :: Maybe Integer
                return $ Number y 

parseExpr :: P.Parser LispVal
parseExpr = parseAtom
         P.<|> parseString
         P.<|> parseNumber
