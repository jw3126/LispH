module Eval(
    Store,
    InterpreterM,
    eval
) where

import Expr
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
-- import Data.Map
import Control.Monad.State.Lazy 

type Errorful t = Either String t
type Store = Map.Map String Ex
type InterpreterM t = StateT Store IO (Errorful t)

todoError :: Ex -> InterpreterM Ex
todoError x = return $ Left $ "TODO: Eval on " ++ (show x) ++ " not yet implemented"

typeError :: Ex -> InterpreterM Ex
typeError x = return $ Left $ "Type error evaluting " ++ (show x)

undefError :: Ex -> InterpreterM Ex
undefError x = return $ Left $ "Undefinded variable " ++ (show x)

lookupInStore :: String -> InterpreterM Ex
lookupInStore s = do
    store <- get
    case Map.lookup s store of
        Nothing -> undefError (ExAtom s)
        Just x -> return $ Right x

insertStore :: String -> Ex -> InterpreterM Ex
insertStore key val = do
    store <- get
    put $ Map.insert key val store
    return $ Right val

builtin = map ExAtom [
    "+"
    , "-"
    , "*"
    , "set"
    , "fn"
--     , "eval"
    ]

eval :: Ex -> InterpreterM Ex
eval x@(ExInteger _) = return $ Right x
eval x@(ExBool _) = return $ Right x
eval x@(ExString _) = return $ Right x
eval x@(ExFunction _ _) = return $ Right x
eval x@(ExAtom s) = if x `elem` builtin then return $ Right x else lookupInStore s
eval x@(ExList (h:t)) = if h `elem` builtin then evalListBuiltin x else evalList x
eval x@(ExList []) = todoError x
eval x = todoError x

evalList :: Ex -> InterpreterM Ex
evalList x@(ExList items) = let 
    a :: [InterpreterM Ex]
    a = map eval items
    b :: StateT Store IO [Either String Ex]
    b = sequence a
    c :: InterpreterM [Ex]
    c = fmap sequence b
    in c >>= (\eargs-> case eargs of 
        Right args -> (eval1 $ ExList args)
        Left msg -> return $ Left $ msg
    )


evalList x = error $ "evalList on " ++ (show x)

evalListBuiltin :: Ex -> InterpreterM Ex
evalListBuiltin (ExList ((ExAtom "fn"):(ExList args):[body])) = return $ Right $ ExFunction args body
evalListBuiltin x@(ExList ((ExAtom "set"):(ExAtom key):[val])) = insertStore key val
evalListBuiltin x@(ExList _) = evalList x
evalListBuiltin x = error $ "evalListBuiltin on " ++ (show x)

-- non recursive evaluation
-- over defined arguments
eval1 :: Ex -> InterpreterM Ex
eval1 x@(ExList []) = todoError x
eval1 (ExList (h:t)) = eval1HeadTail h t
eval1 x = error $ "eval1 got " ++ (show x)

eval1HeadTail :: Ex -> [Ex] -> InterpreterM Ex
eval1HeadTail (ExAtom "quote") t = return $ Right $ ExList t
eval1HeadTail (ExAtom "+") t = return $ evalOp (+) 0 t
eval1HeadTail (ExAtom "*") t = return $ evalOp (*) 1 t
eval1HeadTail (ExAtom "-") t = return $ evalOp (-) 0 t
eval1HeadTail f@(ExFunction _ _) args = eval1Function f args
eval1HeadTail h t = todoError $ ExList (h:t)

liftOpIntInt :: (Integer -> Integer -> Integer) -> (Errorful Ex -> Ex -> Errorful Ex)
liftOpIntInt op a b = 
    case a of
        Left err -> Left err
        Right a0 -> _liftOpIntInt op a0 b

_liftOpIntInt :: (Integer -> Integer -> Integer) -> (Ex -> Ex -> Errorful Ex)
_liftOpIntInt op (ExInteger ia) (ExInteger ib) = Right $ ExInteger $ op ia ib
_liftOpIntInt op a b = error("TODO")

evalOp :: (Integer -> Integer -> Integer) -> Integer -> [Ex] -> Errorful Ex
evalOp op v0 args = foldl (liftOpIntInt op) (Right $ ExInteger v0) args

eval1Function :: Ex -> [Ex] -> InterpreterM Ex
eval1Function (ExFunction sig body) args = if length(sig) == (length(args)) then
        let 
            m = HashMap.fromList $ zip sig args
        in
            eval $ substitute m body
    else
        return $ Left $ "Wrong number of arguments " ++ (show sig) ++ " " ++ (show args)
eval1Function f args  = error $ "Not a function " ++ (show f)

-- TODO quote and functions...
substitute :: (HashMap.HashMap Ex Ex) -> Ex -> Ex
substitute m ex@(ExList items) = ExList $ map (substitute m) items
substitute m ex = HashMap.lookupDefault ex ex m

