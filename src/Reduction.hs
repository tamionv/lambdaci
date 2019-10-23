module Reduction where

import Syntax
import Control.Monad
import Debug.Trace

increment :: String -> String
increment "" = "a"
increment ('z':xs) = 'a':'a':xs
increment ('9':xs) = '0':'0':xs
increment (x:xs) = succ x : xs

incrementTilGood ident term
    | not (ident `elem` freeVars term) = ident
    | otherwise = trace (ident ++ "\n") $ incrementTilGood (increment ident) term

substitute :: Term -> Ident -> Term -> Term
substitute s x t = case t of 
    App a b -> App (substitute s x a) (substitute s x b)
    Abs y a | x == y -> a
            | otherwise -> Abs yy (substitute s x (substitute (Var yy) y a))
        where yy = incrementTilGood y s
    Var y | x == y -> s
          | otherwise -> Var y

leftmostReduce :: Term -> Maybe Term
leftmostReduce term = case term of
    Var x -> Nothing
    Abs x t -> liftM (Abs x) (leftmostReduce t)
    App (Abs x s) t -> Just $ substitute t x s
    App s t -> liftM (flip App t) (leftmostReduce s) `mplus` liftM (App s) (leftmostReduce t)

fullyLeftmostReduce :: Term -> Term
fullyLeftmostReduce term = case leftmostReduce term of
    Nothing -> term
    Just term' -> fullyLeftmostReduce term'

headReduce :: Term -> Maybe Term
headReduce = firstStep where
    firstStep (App s t) = secondStep (App s t)
    firstStep (Abs x s) = firstStep s
    firstStep (Var x)   = Nothing
    secondStep (App (App s t) u) = secondStep (App s t)
    secondStep (App (Abs x s) t) = Just $ substitute t x s
    secondStep (App (Var x) s) = Nothing

fullyHeadReduce :: Term -> Term
fullyHeadReduce term = case headReduce term of
    Nothing -> term
    Just term' -> fullyHeadReduce term'
