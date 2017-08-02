module Expr
    (
     Ex(ExInteger, ExList, ExAtom, ExBool, ExString)
    ) where

data Ex = ExInteger Integer 
    | ExList [Ex] 
    | ExAtom String 
    | ExString String
    | ExBool Bool deriving (Show, Eq)
