module Cli (runCliLoop) where

import Data.Char
import Syntax
import Types
import Reduction
import Control.Monad
import Control.Monad.State.Lazy
import qualified Text.ParserCombinators.ReadP as P

data CliCommand = LeftmostReduce { term :: Term }
                | FullyLeftmostReduce { term :: Term }
                | HeadReduce { term :: Term }
                | FullyHeadReduce { term :: Term } 
                | Type { term :: Term }
                | TypeProof { term :: Term }
                | AssignVariable { ident :: Ident, term :: Term }
    deriving Show

simpleCommand :: String -> (Term -> CliCommand) -> P.ReadP CliCommand
simpleCommand str f = do
    P.string str
    t <- P.munch (/='\n')
    return $ f $ read t

assignment :: P.ReadP CliCommand
assignment = do
    t <- P.munch1 isAlpha
    P.skipSpaces
    P.string "="
    P.skipSpaces
    s <- P.munch (/='\n')
    return $ AssignVariable { ident = t, term = read s }

parseCommand :: P.ReadP CliCommand
parseCommand = assignment
             P.+++ simpleCommand "leftmost reduce" LeftmostReduce
             P.+++ simpleCommand "fully leftmost reduce" FullyLeftmostReduce
             P.+++ simpleCommand "head reduce" HeadReduce
             P.+++ simpleCommand "fully head reduce" FullyHeadReduce
             P.+++ simpleCommand "type proof" TypeProof
             P.<++ simpleCommand "type" Type

instance Read CliCommand where
    readsPrec _ = P.readP_to_S parseCommand

type CliContext = [(Ident, Term)]

addToContext :: (Ident, Term) -> CliContext -> CliContext
addToContext defn@(ident, _) = (defn:) . filter ((/=ident) . fst)

termInContext :: (Monad m) => Term -> StateT CliContext m Term
termInContext t = do
    ctx <- get
    return $ foldr (\(ident, term) current -> substitute term ident current) t ctx

cliAction :: CliCommand -> Term -> String
cliAction (LeftmostReduce _) = show . leftmostReduce
cliAction (FullyLeftmostReduce _) = show . fullyLeftmostReduce
cliAction (HeadReduce _) = show . headReduce
cliAction (Type _) = show . deduceType
cliAction (TypeProof _) = show . deduceTypeProof

doCliCommand :: CliCommand -> StateT CliContext IO ()
doCliCommand (AssignVariable id t) = modify (addToContext (id, t))
doCliCommand (Type x) = case deduceType x of
    Just t -> liftIO $ putStrLn $ show t
    Nothing -> liftIO $ putStrLn "Untypable"
doCliCommand (TypeProof x) = case deduceTypeProof x of
    Just t -> liftIO $ putStrLn $ show t
    Nothing -> liftIO $ putStrLn "Untypable"
doCliCommand command = termInContext (term command) >>= liftIO . putStrLn . cliAction command

getCliCommand :: IO CliCommand
getCliCommand = do
    str <- getLine
    return $ read str

getAndDoCliCommand = liftIO getCliCommand >>= doCliCommand

cliLoop = forever getAndDoCliCommand

runCliLoop :: IO ()
runCliLoop = evalStateT cliLoop [] 
