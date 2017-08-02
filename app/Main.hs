module Main where
import Text.ParserCombinators.Parsec
import Lib

main :: IO ()
main = do
    parseTest expression "( 1 2  3 )"
    parseTest expression "( 1 2 3)"
    parseTest expression "()"
    parseTest expression "(f x (+ 1 2))"
    parseTest expression "( #f )"
