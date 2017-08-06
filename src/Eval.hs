module Eval(
    Store,
    InterpreterM,
    eval,
    Error(..),
    Errorful,
    emptyStore
) where

import Expr
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Control.Monad.State.Lazy 

data Error = TodoError String
    | TypeMismatch Ex String
    | UndefinedVariable Ex
    | UndefindedEval Ex
    | WrongNumberOfArguments [Ex] [Ex]
    | ParserError String
    deriving (Show, Eq)

type Errorful t = Either Error t
type Store = Map.Map String Ex
type InterpreterM t = StateT Store IO (Errorful t)

emptyStore :: Store
emptyStore = Map.empty

lookupInStore :: String -> InterpreterM Ex
lookupInStore s = do
    store <- get
    case Map.lookup s store of
        Nothing -> return $ Left $ UndefinedVariable $ ExSymbol s
        Just x -> return $ Right x

insertStore :: String -> Ex -> InterpreterM Ex
insertStore key val = do
    store <- get
    put $ Map.insert key val store
    return $ Right val

builtinFunctions = map ExSymbol [
    "+"
    , "-"
    , "*"
--     , "eval"
    ]

builtinSpecialForms = map ExSymbol [
    "set"
    , "fn"
    , "quote"
    ]

builtin = builtinFunctions ++ builtinSpecialForms

eval :: Ex -> InterpreterM Ex
eval x@(ExInteger _) = return $ Right x
eval x@(ExBool _) = return $ Right x
eval x@(ExString _) = return $ Right x
eval x@(ExFunction _ _) = return $ Right x
eval x@(ExSymbol s) = if x `elem` builtin then return $ Right x else lookupInStore s
eval x@(ExList (h:t)) | h `elem` builtinFunctions = evalBuiltinFunction x
                      | h `elem` builtinSpecialForms = evalBuiltinSpecialForm x
                      | otherwise = evalList x

eval x@(ExList []) = evalList x
eval x = return $ Left $ TodoError $ show x

-- evaluate a ExList
--
-- The list might start with a special from like "set"
-- in this case, we don't want to evaluate the arguments immediately
--
evalList :: Ex -> InterpreterM Ex
evalList x@(ExList items) = evalListElements items >>= (\eargs-> case eargs of 
        Right (f:args) -> (apply f $ args)
        Right [] -> return $ Left $ TodoError "eval empty list"
        Left err -> return $ Left $ err
    )

evalList x = error $ "evalList on " ++ (show x)

apply :: Ex -> [Ex] -> InterpreterM Ex
apply f@(ExFunction _ _) args = applyFunction f args
apply h t = return $ Left $ TodoError $ "apply" ++ (show h) ++ (show t)

evalListElements :: [Ex] -> InterpreterM [Ex]
evalListElements items = let
    a :: [InterpreterM Ex]
    a = map eval items
    b :: StateT Store IO [Errorful Ex]
    b = sequence a
    c :: InterpreterM [Ex]
    c = fmap sequence b
    in c

evalBuiltinSpecialForm :: Ex -> InterpreterM Ex
evalBuiltinSpecialForm (ExList ((ExSymbol "fn"):(ExList args):[body])) = 
    return $ Right $ ExFunction args body
evalBuiltinSpecialForm x@(ExList ((ExSymbol "set"):(ExSymbol key):[val])) = 
    eval val >>= (\eex -> case eex of
        Right ex -> insertStore key ex
        Left err -> return $ Left err
    )
evalBuiltinSpecialForm x = error $ "evalBuiltinSpecialForm on " ++ (show x)

evalBuiltinFunction :: Ex -> InterpreterM Ex
evalBuiltinFunction x@(ExList (f:args)) = let
    applyf :: Errorful [Ex] -> InterpreterM Ex
    applyf (Right args) = applyBuiltinFunction f args
    in (evalListElements args) >>= applyf

applyBuiltinFunction :: Ex -> [Ex] -> InterpreterM Ex
applyBuiltinFunction (ExSymbol "quote") t = return $ Right $ ExList t
applyBuiltinFunction (ExSymbol "+") t = return $ evalOp (+) 0 t
applyBuiltinFunction (ExSymbol "*") t = return $ evalOp (*) 1 t
applyBuiltinFunction (ExSymbol "-") t = return $ evalOp (-) 0 t

liftOpIntInt :: (Integer -> Integer -> Integer) -> (Errorful Ex -> Ex -> Errorful Ex)
liftOpIntInt op a b = 
    case a of
        Left err -> Left err
        Right a0 -> _liftOpIntInt op a0 b

_liftOpIntInt :: (Integer -> Integer -> Integer) -> (Ex -> Ex -> Errorful Ex)
_liftOpIntInt op (ExInteger ia) (ExInteger ib) = Right $ ExInteger $ op ia ib
_liftOpIntInt op (ExInteger a) b = Left $ TypeMismatch b "Expected Integer"
_liftOpIntInt op a b = Left $ TypeMismatch a "Expected Integer"

evalOp :: (Integer -> Integer -> Integer) -> Integer -> [Ex] -> Errorful Ex
evalOp op v0 args = foldl (liftOpIntInt op) (Right $ ExInteger v0) args

applyFunction :: Ex -> [Ex] -> InterpreterM Ex
applyFunction (ExFunction sig body) args = if length(sig) == (length(args)) then
        let 
            m = HashMap.fromList $ zip sig args
        in
            eval $ substitute m body
    else
        return $ Left $ WrongNumberOfArguments sig args
applyFunction f args  = error $ "Not a function " ++ (show f)

-- TODO quote and functions...
substitute :: (HashMap.HashMap Ex Ex) -> Ex -> Ex
substitute m ex@(ExList items) = ExList $ map (substitute m) items
substitute m ex = HashMap.lookupDefault ex ex m
