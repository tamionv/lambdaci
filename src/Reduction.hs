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
    | ident `elem` freeVars term = incrementTilGood (increment ident) term
    | otherwise = ident

alphaConvert :: Ident -> Ident -> Term -> Term
alphaConvert x xx (App a b) = App (alphaConvert x xx a) (alphaConvert x xx b)
alphaConvert x xx (Abs y a)
    | x == y = Abs y a
    | otherwise = Abs y (alphaConvert x xx a)
alphaConvert x xx (Var y)
    | x == y = Var xx
    | otherwise = Var y

substitute :: Term -> Ident -> Term -> Term
substitute s x (App a b) = App (substitute s x a) (substitute s x b)
substitute s x (Abs y a)
    | x == y = Abs y a
    | otherwise = Abs yy (substitute s x (alphaConvert y yy a))
    where yy = incrementTilGood y s
substitute s x (Var y)
    | x == y = s
    | otherwise = Var y

leftmostReduce :: Term -> Maybe Term
leftmostReduce (Var x) = Nothing
leftmostReduce (Abs x t) = liftM (Abs x) $ leftmostReduce t
leftmostReduce (App (Abs x s) t) = Just $ substitute t x s
leftmostReduce (App s t) = liftM (flip App t) (leftmostReduce s)
                            `mplus` liftM (App s) (leftmostReduce t)

fullyLeftmostReduce :: Term -> Term
fullyLeftmostReduce term = maybe term fullyLeftmostReduce $ leftmostReduce term

headReduce :: Term -> Maybe Term
headReduce = firstStep where
    firstStep (App s t) = secondStep (App s t)
    firstStep (Abs x s) = firstStep s
    firstStep (Var x)   = Nothing
    secondStep (App (App s t) u) = secondStep (App s t)
    secondStep (App (Abs x s) t) = Just $ substitute t x s
    secondStep (App (Var x) s) = Nothing

fullyHeadReduce :: Term -> Term
fullyHeadReduce term = maybe term fullyHeadReduce $ headReduce term
