module Syntax (Ident, Term(App, Abs, Var), freeVars) where

type Ident = String

data Term = App Term Term
          | Abs Ident Term
          | Var Ident

freeVars (Var x) = [x]
freeVars (Abs x s) = filter (/=x) $ freeVars s
freeVars (App s t) = freeVars s ++ freeVars t
