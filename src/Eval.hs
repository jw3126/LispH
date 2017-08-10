module Eval(
    Store,
    InterpreterM,
    throwI,
    evalI,
    exceptI,
    eval,
    emptyStore
) where

import Expr
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Control.Monad.State.Strict
import Control.Monad.Except

type Store = Map.Map String Ex
type InterpreterM = StateT Store (ExceptT Error IO)
-- for reasons I do not understand
--
-- type InterpreterM t = StateT Store (ExceptT Error IO) t
--
-- would cause trouble. See
-- https://stackoverflow.com/questions/9289893/declaring-instances-of-parameterized-type-synonyms
exceptI :: Either Error t -> InterpreterM t
exceptI x = lift inner where
    inner = case x of
        Left err -> throwError err
        Right ex -> return ex

throwI :: Error -> InterpreterM t
throwI err = throwI err

evalI :: InterpreterM t -> Store -> IO (Either Error t)
evalI interpreter store = runExceptT $ evalStateT interpreter store

-- StateT Store IO a == Store -> IO (Store, a)
-- ExceptT Error (StateT Store IO) a == Store -> IO (Either Error a, Store)
-- ExceptT Error IO = IO (Either Error a)
-- StateT Store (ExceptT Error IO) a == Store -> IO (Either Error (a, Store))

emptyStore :: Store
emptyStore = Map.empty


lookupInStore :: String -> InterpreterM Ex
lookupInStore s = get >>= (\store -> let
    exe :: Either Error Ex
    exe = case Map.lookup s store of
            Nothing -> Left $ UndefindedEval $ ExSymbol s
            Just ex -> Right ex
    in
    exceptI exe)

insertStore :: String -> Ex -> InterpreterM Ex
insertStore key val = do
    store <- get
    put $ Map.insert key val store
    return val


builtinSpecialForms = [
    "set"
    , "fn"
    , "quote" -- TODO
    ]

type BuiltinFunction = ([Ex] -> InterpreterM Ex)
type BuiltinFunctionRegistry = HashMap.HashMap String BuiltinFunction
type RHF a b =
    String -> (b -> a -> b) -> b  -> BuiltinFunctionRegistry -> BuiltinFunctionRegistry
-- the _ argument is for guiding type inference
registerHaskellFunction :: (ExAble a, ExAble b) => RHF a b
registerHaskellFunction key f v0 registry = let
    val :: BuiltinFunction
    val = \exs -> exceptI $ (foldlHaskellFunction f v0 exs)
    in HashMap.insert key val registry

rHII :: RHF Integer Integer
rHII = registerHaskellFunction

rHBB :: RHF Bool Bool
rHBB = registerHaskellFunction

builtinFunctionRegistry :: BuiltinFunctionRegistry
builtinFunctionRegistry = let
    registry :: BuiltinFunctionRegistry
    registry = HashMap.empty
    -- we need zero, one for to make typeinference easier
    in
     rHII  "+"     (+)    0
    . rHII "-"     (-)    0
    . rHII "*"     (*)    1
    . rHBB "&&"    (&&)   True
    . rHBB "||"    (||)   False
    $ registry

-- TODO
-- "=="    (==)   True
-- "<"     (<)    True
-- ">"     (>)    True
-- ">="    (>=)   True

builtinFunctions = HashMap.keys builtinFunctionRegistry
builtin = builtinFunctions ++ builtinSpecialForms

eval :: Ex -> InterpreterM Ex
eval x@(ExInteger _) = return x
eval x@(ExBool _) = return x
eval x@(ExString _) = return x
eval x@(ExFunction _ _) = return x
eval x@(ExSymbol s) = if s `elem` builtin then return x else lookupInStore s
eval x@(ExList ((ExSymbol s):t)) | s `elem` builtinFunctions = evalBuiltinFunction x
                      | s `elem` builtinSpecialForms = evalBuiltinSpecialForm x
                      | otherwise = evalList x
eval x@(ExList _) = evalList x
eval x = throwI $ TodoError $ show x

-- evaluate a ExList
--
-- The list might start with a special from like "set"
-- in this case, we don't want to evaluate the arguments immediately
--
evalList :: Ex -> InterpreterM Ex
evalList x@(ExList items) = evalListElements items >>= (\fargs-> case fargs of
        (f:args) -> (apply f args)
        [] -> throwI $ TodoError "eval empty list"
    )

evalList x = error $ "evalList on " ++ (show x)

apply :: Ex -> [Ex] -> InterpreterM Ex
apply f@(ExFunction _ _) args = applyFunction f args
apply h t = throwI $ TodoError $ "apply" ++ (show h) ++ (show t)

evalListElements :: [Ex] -> InterpreterM [Ex]
evalListElements = mapM eval

evalBuiltinSpecialForm :: Ex -> InterpreterM Ex
evalBuiltinSpecialForm (ExList ((ExSymbol "fn"):(ExList args):[body])) =
    return $ ExFunction args body

evalBuiltinSpecialForm x@(ExList ((ExSymbol "set"):(ExSymbol key):[val])) =
    eval val >>= insertStore key

evalBuiltinSpecialForm x = error $ "evalBuiltinSpecialForm on " ++ (show x)

evalBuiltinFunction :: Ex -> InterpreterM Ex
evalBuiltinFunction x@(ExList (f:args)) = evalListElements args >>= applyBuiltinFunction f

applyBuiltinFunction :: Ex -> [Ex] -> InterpreterM Ex
applyBuiltinFunction (ExSymbol s)   t = let
    f :: [Ex] -> InterpreterM Ex
    f = fromJust $ HashMap.lookup s builtinFunctionRegistry
    in f t

applyHaskellFunction :: (ExAble a, ExAble o) => (a -> o) -> Ex -> Errorful Ex
applyHaskellFunction f ex = do
    x <- fromEx ex
    return $ toEx $ f x

applyHaskellFunction2 :: (ExAble a, ExAble b, ExAble o) => (a -> b -> o) -> (Errorful Ex) -> Ex -> Errorful Ex
applyHaskellFunction2 f eex1 ex2 = do
    ex1 <- eex1
    x1 <- fromEx ex1
    x2 <- fromEx ex2
    return $ toEx $ f x1 x2

foldlHaskellFunction :: (ExAble a, ExAble b) => (b -> a -> b) -> b -> [Ex] -> Errorful Ex
foldlHaskellFunction f v0 exs =
    foldl (applyHaskellFunction2 f) (Right $ toEx v0) exs

applyFunction :: Ex -> [Ex] -> InterpreterM Ex
applyFunction (ExFunction sig body) args = if length sig == length args then
        let
            m = HashMap.fromList $ zip sig args
        in
            eval $ substitute m body
    else
        throwI $ WrongNumberOfArguments sig args
applyFunction f args  = error $ "Not a function " ++ (show f)

-- TODO quote and functions...
-- https://en.wikipedia.org/wiki/De_Bruijn_index
substitute :: (HashMap.HashMap Ex Ex) -> Ex -> Ex
substitute m ex@(ExList items) = ExList $ map (substitute m) items
substitute m ex = HashMap.lookupDefault ex ex m
