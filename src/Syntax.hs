module Syntax (Ident, Term(App, Abs, Var), freeVars) where

type Ident = String

data Term = App Term Term
          | Abs Ident Term
          | Var Ident

freeVars term = case term of
    Var x -> [x]
    Abs x s -> filter (/=x) $ freeVars s
    App s t -> freeVars s ++ freeVars t

