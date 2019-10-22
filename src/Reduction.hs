module Reduction where

import Syntax
import Control.Monad

substitute :: Term -> Ident -> Term -> Term
substitute s x t = case s of 
    App a b -> App (substitute a x t) (substitute b x t)
    Abs y a | x == y -> a
            | otherwise -> Abs y (substitute a x t)
    Var y | x == y -> t
          | otherwise -> Var y

leftmostReduce :: Term -> Maybe Term
leftmostReduce term = case term of
    Var x -> Nothing
    Abs x t -> liftM (Abs x) (leftmostReduce t)
    App (Abs x s) t -> Just $ substitute t x s
    App s t -> liftM (flip App t) (leftmostReduce s) `mplus` liftM (App s) (leftmostReduce t)

headReduce :: Term -> Maybe Term
headReduce = firstStep where
    firstStep (App s t) = secondStep (App s t)
    firstStep (Abs x s) = firstStep s
    firstStep (Var x)   = Nothing
    secondStep (App (App s t) u) = secondStep (App s t)
    secondStep (App (Abs x s) t) = Just $ substitute t x s
    secondStep (App (Var x) s) = Nothing
