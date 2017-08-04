module Expr
    (
     Ex(ExInteger, ExList, ExAtom, ExBool, ExString, ExFunction)
    , EvalResult
    , toString
    , falseString, trueString
    ) where

import Data.List
import Data.Hashable

type EvalResult = Either String Ex

data Ex = ExInteger Integer 
    | ExList [Ex] 
    | ExAtom String 
    | ExString String
    | ExFunction [Ex] Ex
    | ExBool Bool deriving (Show, Eq)

-- TODO is there an automatic way to do this?
instance Hashable Ex where
    hashWithSalt salt (ExList inner) = hashWithSalt salt inner
    hashWithSalt salt (ExAtom inner) = hashWithSalt salt inner
    hashWithSalt salt (ExString inner) = hashWithSalt salt inner
    hashWithSalt salt (ExBool inner) = hashWithSalt salt inner
    hashWithSalt salt (ExFunction args body) = 
        hashWithSalt (hashWithSalt salt args) body

falseString = "#f"
trueString = "#t"

toString :: Ex -> String
toString (ExInteger i) = show i
toString (ExAtom s) = s
toString (ExBool False) = falseString
toString (ExBool True) = trueString
toString (ExString s) = "\"" ++ s ++ "\""
toString (ExList items) = "(" ++ (intercalate " " $ map toString items) ++ ")"
toString (ExFunction args body) = toString $ ExList $ [ExAtom "fn", ExList args, body]
