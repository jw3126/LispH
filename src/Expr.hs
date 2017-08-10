{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Expr
    (Ex(..)
    , Error(..)
    , Errorful
    , EvalResult
    , ExAble
    , toString
    , falseString, trueString
    , fromEx, toEx
    ) where

import Data.Hashable
import Data.List (intercalate)
import GHC.Generics (Generic)

type EvalResult = Either String Ex

data Ex = ExInteger Integer
    | ExList [Ex]
    | ExSymbol String -- TODO ExVar
    | ExString String
    | ExFunction [Ex] Ex --TODO (String, Maybe Ex)
    | ExBool Bool deriving (Show, Eq, Generic)

instance Hashable Ex

data Error = TodoError String
    | TypeMismatch Ex String
    | UndefinedVariable Ex
    | UndefindedEval Ex
    | WrongNumberOfArguments [Ex] [Ex]
    | ParserError String
    deriving (Show, Eq)

type Errorful t = Either Error t

class ExAble t where
    toEx :: t -> Ex
    fromEx :: Ex -> Errorful t

-- is there a way to do this with less boiler plate?
instance ExAble Integer where
    toEx = ExInteger
    fromEx (ExInteger i) = Right i
    fromEx ex = Left $ TypeMismatch ex "Integer"

instance ExAble Bool where
    toEx = ExBool
    fromEx (ExBool b) = Right b
    fromEx ex = Left $ TypeMismatch ex "Bool"

instance ExAble String where
    toEx = ExString
    fromEx (ExString s) = Right s
    fromEx ex = Left $ TypeMismatch ex "String"


falseString = "#f"
trueString = "#t"

toString :: Ex -> String
toString (ExInteger i) = show i
toString (ExSymbol s) = s
toString (ExBool False) = falseString
toString (ExBool True) = trueString
toString (ExString s) = "\"" ++ s ++ "\""
toString (ExList items) = "(" ++ unwords (map toString items) ++ ")"
toString (ExFunction args body) = toString $ ExList [ExSymbol "fn", ExList args, body]
