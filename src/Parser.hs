module Parser () where

import Syntax
import Text.ParserCombinators.ReadP
import Control.Monad
import Data.Char

instance Show Term where
    show term = case term of
        App s t -> "(" ++ show s ++ show t ++ ")"
        Abs x t -> "(" ++ "\\" ++ x ++ "." ++ show t ++ ")"
        Var x -> "(" ++ x ++ ")"

ident :: ReadP Ident
ident = munch isAlpha

parens :: ReadP a -> ReadP a
parens x = do
    skipSpaces
    char '('
    r <- x
    skipSpaces
    char ')'
    return r

app :: ReadP Term
app = liftM2 App term term

abs :: ReadP Term
abs = liftM2 Abs (skipSpaces >> char '\\' >> ident) (skipSpaces >> char '.' >> term)

var :: ReadP Term
var = Var <$> ident

term = (parens app +++ parens Parser.abs +++ parens var)

instance Read Term where
    readsPrec _ = readP_to_S term
