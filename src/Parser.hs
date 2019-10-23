module Parser () where

import Syntax
import Text.ParserCombinators.ReadP
import Control.Monad
import Data.Char

instance Show Term where
    show term = case term of
        App (Var x) (Var y) -> x ++ " " ++ y
        App (Var x) y -> x ++ "(" ++ show y ++ ")"
        App (abs@(Abs _ _)) (Var y) -> "(" ++ show abs ++ ")" ++ y
        App (abs@(Abs _ _)) y -> "(" ++ show abs ++ ")" ++ "(" ++ show y ++ ")"
        App (app@(App _ (Var _))) (Var z) -> show app ++ " " ++ z
        App (app@(App _ _)) (Var z) -> show app ++ z
        App (app@(App _ _)) z -> show app ++ "(" ++ show z ++ ")"
        Abs x t -> "\\" ++ x ++ "." ++ show t
        Var x -> x

ident :: ReadP Ident
ident = munch1 isAlphaNum

parens :: ReadP a -> ReadP a
parens x = do
    skipSpaces
    char '('
    r <- x
    skipSpaces
    char ')'
    return r

var :: ReadP Term
var = skipSpaces >> Var <$> ident

appRhs :: ReadP Term
appRhs = var +++ parens Parser.abs +++ parens app

varLhsApp :: ReadP Term
varLhsApp = liftM2 App var appRhs

absLhsApp :: ReadP Term
absLhsApp = liftM2 App (parens Parser.abs) appRhs

remainingApp :: Term -> ReadP Term
remainingApp current = liftM (App current) $ (parens term +++ var)

appLhsApp :: ReadP Term
appLhsApp = (varLhsApp +++ absLhsApp +++ parens app) >>= remainingApp

abs :: ReadP Term
abs = do
    skipSpaces
    char '\\'
    x <- ident
    skipSpaces
    char '.'
    t <- term
    return $ Abs x t

app :: ReadP Term
app = varLhsApp +++ appLhsApp +++ absLhsApp
 
term :: ReadP Term
term = var +++ app +++Parser.abs

instance Read Term where
    readsPrec _ = readP_to_S term
