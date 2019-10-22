module Types (TypeContext, TypeIdent, Type, TypeProof, deduceTypeProof, deduceType) where

import Syntax
import Parser
import Data.Maybe
import Control.Monad.State.Lazy
import Data.List
import Debug.Trace

type TypeIdent = String

data Type = TVar TypeIdent
          | Arrow Type Type deriving(Eq, Ord)

instance Show Type where
    show (TVar x) = x
    show (Arrow (TVar x) y) = x ++ " -> " ++ show y
    show (Arrow x y) = "(" ++ show x ++ ") -> " ++ show y

type TypeContext = [(Ident, Type)]

data TypeProof = Assumption { context :: TypeContext
                            , term :: Term
                            , termType :: Type
                            }
               | Abstraction { context :: TypeContext
                             , term :: Term
                             , termType :: Type
                             , just :: TypeProof
                             }
               | Application { context :: TypeContext
                             , term :: Term
                             , termType :: Type
                             , just1 :: TypeProof
                             , just2 :: TypeProof
                             }

formatContext :: TypeContext -> String
formatContext xs = "{ " ++ intercalate ", " (map (\(id, t) -> id ++ ": " ++ show t) xs) ++ " }"

formatTypeProof :: TypeProof -> State Int String
formatTypeProof (Assumption ctx s t) = do
    modify (+1)
    ln <- get
    return $ show ln ++ ". " ++ formatContext ctx ++ " |- " ++ show s ++ ": " ++ show t ++ " (assumption)"
formatTypeProof (Abstraction ctx term t tp') = do
    previous <- formatTypeProof tp'
    justLn <- get
    modify (+1)
    ln <- get
    return $ concat [ previous
                    , "\n"
                    , show ln
                    , ". "
                    , formatContext ctx
                    , " |- "
                    , show term
                    , ": "
                    , show t
                    , " (abstraction "
                    , show justLn
                    , ")"
                    ]
formatTypeProof (Application ctx term t tp' tp'') = do
    previous <- formatTypeProof tp'
    justLn <- get
    previous' <- formatTypeProof tp''
    justLn' <- get
    modify (+1)
    ln <- get
    return $ concat [ previous
                    , "\n"
                    , previous'
                    , "\n"
                    , show ln
                    , ". "
                    , formatContext ctx
                    , " |- "
                    , show term
                    , ": "
                    , show t
                    , " (application "
                    , show justLn
                    , ", "
                    , show justLn'
                    , ")"
                    ]

instance Show TypeProof where
    show tp = evalState (formatTypeProof tp) 0

freeTVars :: Type -> [Ident]
freeTVars (TVar x) = [x]
freeTVars (Arrow x y) = freeTVars x ++ freeTVars y

type Substition = [(TypeIdent, Type)]

applySub :: Substition -> Type -> Type
applySub [] t = t
applySub ((ident, t):subs) (TVar x)
    | x == ident = applySub subs t
    | otherwise  = applySub subs (TVar x)
applySub sub (Arrow x y) = Arrow (applySub sub x) (applySub sub y)

applySubCtx sub ctx = map f ctx
    where f (a, b) = (a, applySub sub b)

applySubTp :: Substition -> TypeProof -> TypeProof
applySubTp sub (Assumption ctx term t)
    = Assumption (applySubCtx sub ctx) term (applySub sub t)
applySubTp sub (Abstraction ctx term t tp)
    = Abstraction (applySubCtx sub ctx) term (applySub sub t) (applySubTp sub tp)
applySubTp sub (Application ctx term t tp tp')
    = Application (applySubCtx sub ctx) term (applySub sub t) (applySubTp sub tp) (applySubTp sub tp')

freshTVarIdent :: StateT [TypeIdent] Maybe TypeIdent
freshTVarIdent = do
    ret <- head <$> get
    modify tail
    return ret

findMGUSubstitution :: Type -> Type -> Maybe Substition
findMGUSubstitution (TVar x) (TVar y)
    | x == y    = Just []
    | otherwise = Just [(x, TVar y)]
findMGUSubstitution (TVar x) y
    | x `elem` freeTVars y = Nothing
    | otherwise            = Just [(x, y)]
findMGUSubstitution (Arrow x y) (Arrow z t) = do
    a <- findMGUSubstitution x z
    b <- findMGUSubstitution y t
    return $ take 1 $ a ++ b
findMGUSubstitution s t = findMGUSubstitution t s

findMGUPair :: Type -> Type -> Maybe Substition
findMGUPair s t
    | s == t = Just []
    | otherwise = do
        sub <- findMGUSubstitution s t
        mgu <- findMGUPair (applySub sub s) (applySub sub t)
        return $ sub ++ mgu

findMGU :: [Type] -> [Type] -> Maybe Substition
findMGU xs ys = findMGUPair xArrows yArrows where
    xArrows = f xs
    yArrows = f ys
    f = foldr1 Arrow

deduceTypeProof :: Term -> Maybe TypeProof
deduceTypeProof term = evalStateT (rec term) $ [1..] >>= (`replicateM` ['a'..'z']) where
    rec :: Term -> StateT [String] Maybe TypeProof
    rec term@(Var x) = do
        t <- TVar <$> freshTVarIdent
        return $ Assumption [(x, t)] term t
    rec term@(Abs x s) = do
        let fv = freeVars s
        tps <- rec s
        let ctx = context tps
        let finalCtx = filter ((/=x) . fst) ctx
        tx <- if x `elem` fv then lift $ lookup x ctx else TVar <$> freshTVarIdent
        return $ Abstraction finalCtx term (Arrow tx (termType tps)) tps
    rec term@(App s t) = do
        tps <- rec s
        tpt <- rec t
        let ts = termType tps
        let tt = termType tpt
        let ctxs = context tps
        let ctxt = context tpt
        let commonVariables = intersect (freeVars s) (freeVars t)
        let commonCtxS = filter (\(p, q) -> p `elem` commonVariables) ctxs
        let commonCtxT = filter (\(p, q) -> p `elem` commonVariables) ctxt
        let onlySCtx = ctxs \\ commonCtxS
        let onlyTCtx = ctxt \\ commonCtxT
        let ctxAssignS = map snd $ sort commonCtxS
        let ctxAssignT = map snd $ sort commonCtxT
        case ts of
            Arrow p q -> do
                mgu <- lift $ findMGU (p : ctxAssignS) (tt : ctxAssignT)
                return $ applySubTp mgu $ Application (onlySCtx ++ onlyTCtx ++ commonCtxS) term q tps tpt
            TVar name -> do
                tret <- TVar <$> freshTVarIdent
                mgu <- lift $ findMGU (ts : ctxAssignS) (Arrow tt tret : ctxAssignT)
                return $ applySubTp mgu $ Application (onlySCtx ++ onlyTCtx ++ commonCtxS) term tret tps tpt

deduceType :: Term -> Maybe Type
deduceType term = termType <$> deduceTypeProof term
