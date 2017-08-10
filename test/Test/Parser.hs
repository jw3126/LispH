module Test.Parser
    (testParserMain)
where

import Test.HUnit
import Parser
import Expr
import Test.QuickCheck
import Control.Monad

arbitraryExBool :: Gen Ex
arbitraryExBool = do
    b <- arbitrary
    return $ ExBool b

arbitraryExInteger :: Gen Ex
arbitraryExInteger = arbitrary >>= (\x -> return $ ExInteger x)

arbitraryExString :: Gen Ex
arbitraryExString = oneof $ map (return ) [
    -- TODO escaping does not work yet
    ExString "hello"
    , ExString ""
    , ExString "sdasdfsdf34324234"
    ]

arbitraryExSymbol :: Gen Ex
arbitraryExSymbol = fmap ExSymbol arbitraryExSymbolIdentifier

arbitraryExSymbolIdentifier :: Gen String
arbitraryExSymbolIdentifier = oneof [
    return "a"
  , return "a23"
  , return "a?"
  , return "someLongAndUglyNameWith3123123Numbers>>!?"
    ]

arbitraryExList :: Gen Ex
arbitraryExList = fmap ExList arbitrary

arbitrarySizedEx :: Integer -> Gen Ex
arbitrarySizedEx n = oneof [arbitraryExInteger
    , arbitraryExBool
    , arbitraryExString
    , arbitraryExSymbol
    , arbitrarySizedExList n
    ]

arbitrarySizedExList :: Integer -> Gen Ex
arbitrarySizedExList 0 = return $ ExList []
arbitrarySizedExList n = do
    k <- choose(0, 8)
    items <- vectorOf k (arbitrarySizedEx (n-1))
    return $ ExList items

arbitraryEx = arbitrarySizedEx 4

instance Arbitrary Ex where
    arbitrary = arbitraryEx

prop_inverse :: Ex -> Bool
prop_inverse ex = case (parseEx $ toString ex) of
    Left err -> False
    Right ans -> (ex == ans)

testParse :: String -> Ex -> IO ()
testParse input expected = case parseEx input of
    Left err -> print err
    Right ans -> (assertEqual "fail in parsing expression" ans expected)

testParserMain :: IO ()
testParserMain = do
    testParse "1" (ExInteger 1)
    testParse "  12 " (ExInteger 12)
    testParse "(1)" (ExList [ExInteger 1])
    testParse "(foo 1 #t )" (ExList [ExSymbol "foo", ExInteger 1, ExBool True])
    testParse "#f" (ExBool False)
    testParse " #f " (ExBool False)
    testParse "(foo 1 #t)" (ExList [ExSymbol "foo", ExInteger 1, ExBool True])
    testParse "(foo? 112 #t)" (ExList [ExSymbol "foo?", ExInteger 112, ExBool True])
    testParse "(f \"abc\")" (ExList [ExSymbol "f", ExString "abc"])
    testParse "     (+ 1     1)" (ExList [ExSymbol "+", ExInteger 1, ExInteger 1])

    ex <- generate arbitraryEx
    putStrLn $ show ex
    putStrLn $ toString ex
    quickCheck prop_inverse
