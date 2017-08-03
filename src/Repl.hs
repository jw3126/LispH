module Repl(
    repl
) where

import Parser
import Expr
import Eval
import System.IO

repl :: IO ()
repl = do
    repl1
    repl
    
readEx :: IO Ex
readEx = do
    line <- getLine
    case parseEx line of
        Left err -> do 
            print err
            readEx
        Right ex -> return ex

repl1 :: IO ()
repl1 = do
    putStr "lisp>"
    hFlush stdout
    ex <- readEx
    putStrLn $ show ex
    putStrLn $ toString ex
    putStrLn $ toString $ eval ex
