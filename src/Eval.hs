module Eval(
    eval
) where

import Expr
import qualified Data.HashMap.Strict as HashMap

evalNotImplementedError :: Ex -> EvalResult
evalNotImplementedError x = Left $ "Eval on " ++ (show x) ++ " not yet implemented"

typeError :: Ex -> EvalResult
typeError x = Left $ "Type error evaluting " ++ (show x)

undefVarError x = Left $ "Undefinded variable " ++ (show x)

builtin = map ExAtom [
    "+"
    , "-"
    , "*"
    , "set"
    , "fn"
    ]

eval :: Ex -> EvalResult
eval x@(ExInteger _) = Right x
eval x@(ExBool _) = Right x
eval x@(ExString _) = Right x
eval x@(ExFunction _ _) = Right x
eval x@(ExAtom _) = if x `elem` builtin then Right x else undefVarError x
eval x@(ExList (h:t)) = if h `elem` builtin then evalListBuiltin x else evalList x
eval x@(ExList []) = evalNotImplementedError x
eval x = evalNotImplementedError x

evalList :: Ex -> EvalResult
evalList x@(ExList items) = let
    a :: Either String [Ex]
    a = sequence $ map eval items
    in case a of
        Left msg -> Left msg
        Right fargs -> eval1 $ ExList fargs
evalList x = error $ "evalList on " ++ (show x)

evalListBuiltin :: Ex -> EvalResult
evalListBuiltin (ExList ((ExAtom "fn"):(ExList args):[body])) = Right $ ExFunction args body
evalListBuiltin x@(ExList ((ExAtom "set"):(ExAtom key):[val])) = evalNotImplementedError x
evalListBuiltin x@(ExList _) = evalList x
evalListBuiltin x = error $ "evalListBuiltin on " ++ (show x)

-- non recursive evaluation
-- over defined arguments
eval1 :: Ex -> EvalResult
eval1 (ExList []) = Right $ ExList []
eval1 (ExList (h:t)) = eval1HeadTail h t
eval1 x = error $ "eval1 got " ++ (show x)

eval1HeadTail :: Ex -> [Ex] -> EvalResult
eval1HeadTail (ExAtom "quote") t = Right $ ExList t
eval1HeadTail (ExAtom "+") t = evalOp (+) 0 t
eval1HeadTail (ExAtom "*") t = evalOp (*) 1 t
eval1HeadTail (ExAtom "-") t = evalOp (-) 0 t
eval1HeadTail f@(ExFunction _ _) args = eval1Function f args
eval1HeadTail h t = evalNotImplementedError $ ExList (h:t)

liftOpIntInt :: (Integer -> Integer -> Integer) -> (EvalResult -> Ex -> EvalResult)
liftOpIntInt op a b = 
    case a of
        Left err -> Left err
        Right a0 -> _liftOpIntInt op a0 b

_liftOpIntInt :: (Integer -> Integer -> Integer) -> (Ex -> Ex -> EvalResult)
_liftOpIntInt op (ExInteger ia) (ExInteger ib) = Right $ ExInteger $ op ia ib
_liftOpIntInt op a b = error("TODO")

evalOp :: (Integer -> Integer -> Integer) -> Integer -> [Ex] -> EvalResult
evalOp op v0 args = foldl (liftOpIntInt op) (Right $ ExInteger v0) args

eval1Function :: Ex -> [Ex] -> EvalResult
eval1Function (ExFunction sig body) args = if length(sig) == (length(args)) then
        let 
            m = HashMap.fromList $ zip sig args
        in
            eval $ substitute m body
    else
        Left $ "Wrong number of arguments " ++ (show sig) ++ " " ++ (show args)
eval1Function f args  = error $ "Not a function " ++ (show f)

-- TODO quote and functions...
substitute :: (HashMap.HashMap Ex Ex) -> Ex -> Ex
substitute m ex@(ExList items) = ExList $ map (substitute m) items
substitute m ex = HashMap.lookupDefault ex ex m

