module Eval(
    eval
) where

import Expr

type EvalResult = Either String Ex

evalNotImplementedError :: Ex -> EvalResult
evalNotImplementedError x = Left $ "Eval on " ++ (show x) ++ " not yet implemented"

typeError :: Ex -> EvalResult
typeError x = Left $ "Type error evaluting " ++ (show x)

undefVarError x = Left $ "Undefinded variable " ++ (show x)

buildInFunctions = map ExAtom [
    "+"
    , "-"
    , "*"
    ]


eval :: Ex -> EvalResult
eval x@(ExInteger _) = Right x
eval x@(ExBool _) = Right x
eval x@(ExString _) = Right x
eval x@(ExAtom _) = if x `elem` buildInFunctions then Right x else undefVarError x
eval x@(ExList items) = let
    a :: Either String [Ex]
    a = sequence $ map eval items
    in case a of
        Left msg -> Left msg
        Right args -> eval1 $ ExList args

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

