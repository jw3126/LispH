module Parser
    ( expression,
    parseEx
    ) where

import Expr
import Text.ParserCombinators.Parsec
import Control.Monad

parseEx s = parse expression "" s

expression :: Parser Ex
expression = do 
    skipMany space
    (try list) <|> (try integer) <|> (try atomOrBool) <|> (try Parser.string)

integer :: Parser Ex
integer = do
    skipMany space
    s <- many1 digit
    return (ExInteger (read s))

list :: Parser Ex
list = do
    skipMany space
    char '('
    skipMany space
    args <- many (try expression)
    skipMany space
    char ')'
    return (ExList args)

--these are the allowed chars in identifiers
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

digestAtomOrBool :: String -> Ex
digestAtomOrBool  s = case s of
               "#t" -> ExBool True
               "#f" -> ExBool False
               otherwise -> ExAtom s

atomOrBool ::Parser Ex
atomOrBool = do
    skipMany space
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    return $ digestAtomOrBool (first:rest)

string :: Parser Ex
string = do
    -- TODO escaping
    char '"'
    content <- many $ noneOf "\"\n"
    char '"'
    return $ ExString content

