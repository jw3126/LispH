module Main where
import Repl
import Eval
import Data.Map
import Control.Monad.State.Lazy 

store::Store
store = empty

main = evalStateT repl store
