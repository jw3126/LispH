module Repl(
    mainrepl,
    evalString
) where

import Parser
import Expr
import Eval
import System.IO
import Control.Monad
import Control.Monad.State.Lazy
import Control.Monad.Except

repl :: InterpreterM Ex
repl = forever repl1

mainrepl :: IO ()
mainrepl = (evalI repl emptyStore) >> return ()

repl1 :: InterpreterM Ex
repl1 = do
    liftIO $ putStr "lisp>"
    liftIO $ hFlush stdout
    ex <- liftIO readEx
--     liftIO $ putStrLn $ show ex
--     liftIO $ putStrLn $ toString ex
    exres <- eval ex
--     liftIO $ putStrLn $ show exres
    liftIO $ putStrLn $ toString $ exres
    return exres

readEx :: IO Ex
readEx = do
    line <- getLine
    case parseEx line of
        Left err -> do 
            print err
            readEx
        Right ex -> return ex

evalString :: String -> InterpreterM Ex
evalString s = case parseEx s of
    Left err -> throwI $ ParserError $ show err
    Right ex -> eval ex
