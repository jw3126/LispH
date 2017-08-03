module Eval(
    eval
) where

import Expr

eval :: Ex -> Ex
eval x@(ExInteger _) = x
eval x@(ExBool _) = x
eval x@(ExString _) = x
eval (ExList (h:t)) = evalHeadTail h t

evalHeadTail (ExAtom "quote") t = ExList t
evalHeadTail (ExAtom "+") t = evalOp (+) 0 $ map eval t
evalHeadTail (ExAtom "*") t = evalOp (*) 1 $ map eval t
evalHeadTail (ExAtom "-") t = evalOp (-) 0 $ map eval t

liftIntegerOp op (ExInteger a) (ExInteger b) = ExInteger $ op a b

evalOp :: (Integer -> Integer -> Integer) -> Integer -> [Ex] -> Ex
evalOp op v0 args = foldl (liftIntegerOp op) (ExInteger v0) args

