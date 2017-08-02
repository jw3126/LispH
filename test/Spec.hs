import Test.HUnit
import Parser
import Expr
import Text.ParserCombinators.Parsec

testParse :: String -> Ex -> IO ()
testParse input expected = case parse expression "" input of
    Left err -> print err
    Right ans -> (assertEqual "fail in parsing expression" ans expected)

main :: IO ()
main = do
    testParse "1" (ExInteger 1)
    testParse "  12 " (ExInteger 12)
    testParse "(1)" (ExList [ExInteger 1])
    testParse "(foo 1 #t )" (ExList [ExAtom "foo", ExInteger 1, ExBool True])
    testParse "#f" (ExBool False)
    testParse " #f " (ExBool False)
    testParse "(foo 1 #t)" (ExList [ExAtom "foo", ExInteger 1, ExBool True])
    testParse "(foo? 112 #t)" (ExList [ExAtom "foo?", ExInteger 112, ExBool True])
    testParse "(f \"abc\")" (ExList [ExAtom "f", ExString "abc"])


