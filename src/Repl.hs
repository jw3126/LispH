module Repl(
    repl
) where

import Parser
import Expr

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
    ex <- readEx
    print $ show ex
    print $ toString ex
