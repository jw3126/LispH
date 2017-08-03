module Expr
    (
     Ex(ExInteger, ExList, ExAtom, ExBool, ExString)
    , toString
    , falseString, trueString
    ) where

import Data.List

data Ex = ExInteger Integer 
    | ExList [Ex] 
    | ExAtom String 
    | ExString String
    | ExBool Bool deriving (Show, Eq)

falseString = "#f"
trueString = "#t"

toString :: Ex -> String
toString (ExInteger i) = show i
toString (ExAtom s) = s
toString (ExBool False) = falseString
toString (ExBool True) = trueString
toString (ExString s) = "\"" ++ s ++ "\""
toString (ExList items) = "(" ++ (intercalate " " $ map toString items) ++ ")"
