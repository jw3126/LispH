{-# LANGUAGE DeriveGeneric #-}

module Expr
    (
     Ex(ExInteger, ExList, ExSymbol, ExBool, ExString, ExFunction)
    , EvalResult
    , toString
    , falseString, trueString
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

falseString = "#f"
trueString = "#t"

toString :: Ex -> String
toString (ExInteger i) = show i
toString (ExSymbol s) = s
toString (ExBool False) = falseString
toString (ExBool True) = trueString
toString (ExString s) = "\"" ++ s ++ "\""
toString (ExList items) = "(" ++ (intercalate " " $ map toString items) ++ ")"
toString (ExFunction args body) = toString $ ExList $ [ExSymbol "fn", ExList args, body]
