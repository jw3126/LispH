module Test.Eval
    (testEvalMain)
where

import Test.HUnit
import Parser
import Expr
import Eval
import Repl
import Control.Monad
import Control.Monad.State.Lazy

testEvalMain :: IO ()
testEvalMain =
    evalI testM emptyStore >>
    putStrLn "testEvalMain"

testEvalString :: String -> Ex -> InterpreterM ()
testEvalString s expected = do
    ans <- evalString s
    liftIO $ assertEqual s ans expected
    -- return $ Right ()

testM :: InterpreterM ()
testM = do
    testEvalString "1" $ ExInteger 1
    testEvalString "(+ 1 2 3)" $ ExInteger 6
    testEvalString "(+ (+ 1 2) 10)" $ ExInteger 13

    -- function
    testEvalString "((fn (a) (+ a 1)) 10)" $ ExInteger 11

    -- set
    testEvalString "(set x 4)" $ ExInteger 4
    testEvalString "x" $ ExInteger 4
    evalString "(set x 5)"
    testEvalString "x" $ ExInteger 5
    evalString "(set x (+ 1 1))"
    testEvalString "x" $ ExInteger 2

    evalString "(set sq (fn (a) (* a a)))"
    testEvalString "(sq 3)" $ ExInteger 9

    -- function argument shadows variable
    evalString "(set x (+ 1 1))"
    evalString "(set cub (fn (x) (* x x x)))"
    testEvalString "(cub 3)" $ ExInteger 27
    testEvalString "x" $ ExInteger 2
    testEvalString "(cub x)" $ ExInteger 8

    -- higher order function
    evalString "(set adder (fn (n) (fn (x) (+ x n))))"
    testEvalString "((adder 1) 2)" $ ExInteger 3

    -- builtin functions

    testEvalString "(+)" $ ExInteger 0
    testEvalString "(-)" $ ExInteger 0
    testEvalString "(*)" $ ExInteger 1
    testEvalString "(&&)" $ ExBool True
    testEvalString "(||)" $ ExBool False

    testEvalString "(+ 1 2 3)"  $ ExInteger 6
    testEvalString "(|| #t #f)" $ ExBool True
    testEvalString "(&& #t #f)" $ ExBool False
