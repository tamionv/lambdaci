module Types where

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
    show t = case t of
        TVar x -> x
        Arrow (TVar x) y -> x ++ " -> " ++ show y
        Arrow x y -> "(" ++ show x ++ ") -> " ++ show y

type Context = [(Ident, Type)]

data TypeProof = Assumption Context Term Type
               | Abstraction Context Term Type TypeProof
               | Application Context Term Type TypeProof TypeProof

typeFromTypeProof :: TypeProof -> Type
typeFromTypeProof tp = case tp of
    Assumption _ _ t -> t
    Abstraction _ _ t _ -> t
    Application _ _ t _ _ -> t

ctxFromTypeProof tp = case tp of
    Assumption ctx _ _ -> ctx
    Abstraction ctx _ _ _ -> ctx
    Application ctx _ _ _ _ -> ctx

formatContext :: Context -> String
formatContext xs = "{ " ++ intercalate ", " (map (\(id, t) -> id ++ ": " ++ show t) xs) ++ " }"

formatTypeProof :: TypeProof -> State Int String
formatTypeProof tp = case tp of
    Assumption ctx (Var x) t -> do
        modify (+1)
        ln <- get
        return $ show ln ++ ". " ++ formatContext ctx ++ " |- " ++ x ++ ": " ++ show t ++ " (assumption)"
    Abstraction ctx term t tp' -> do
        previous <- formatTypeProof tp'
        justification <- get
        modify (+1)
        ln <- get
        return $ previous ++ "\n" ++ show ln ++ ". " ++ formatContext ctx ++ " |- " ++ show term ++ ": " ++ show t ++ " (abstraction " ++ show justification ++ ")"
    Application ctx term t tp' tp'' -> do
        previous <- formatTypeProof tp'
        justification <- get
        previous' <- formatTypeProof tp''
        justification' <- get
        modify (+1)
        ln <- get
        return $ previous ++ "\n" ++ previous' ++ "\n" ++ show ln ++ ". " ++ formatContext ctx ++ " |- " ++ show term ++ ": " ++ show t ++ " (application " ++ show justification ++ ", " ++ show justification' ++ ")"

instance Show TypeProof where
    show tp = evalState (formatTypeProof tp) 0

freeTVars :: Type -> [Ident]
freeTVars t = case t of
    TVar x -> [x]
    Arrow x y -> freeTVars x ++ freeTVars y

type Substition = [(TypeIdent, Type)]

applySub :: Substition -> Type -> Type
applySub sub t = case (sub, t) of
    ([], _) -> t
    ((ident, subst):subs, TVar x) | x == ident -> applySub subs subst
                                  | otherwise -> applySub subs (TVar x)
    (sub, Arrow x y) -> Arrow (applySub sub x) (applySub sub y)

applySubCtx sub ctx = map f ctx
    where f (a, b) = (a, applySub sub b)

applySubTp :: Substition -> TypeProof -> TypeProof
applySubTp sub tp = case tp of
    Assumption ctx term t -> Assumption (applySubCtx sub ctx) term (applySub sub t)
    Abstraction ctx term t tp -> Abstraction (applySubCtx sub ctx) term (applySub sub t) (applySubTp sub tp)
    Application ctx term t tp tp' -> Application (applySubCtx sub ctx) term (applySub sub t) (applySubTp sub tp) (applySubTp sub tp')

freshTVarIdent :: StateT [String] Maybe Ident
freshTVarIdent = do
    ret <- head <$> get
    modify tail
    return ret

findMGUSubstitution :: Type -> Type -> Maybe Substition
findMGUSubstitution s t = case (s, t) of
    (TVar x, TVar y) | x == y -> Just []
                     | otherwise -> Just [(x, TVar y)]
    (TVar x, y) | x `elem` freeTVars y -> Nothing
                | otherwise -> Just [(x, y)]
    (Arrow x y, Arrow z t) -> do
        a <- findMGUSubstitution x z
        b <- findMGUSubstitution y t
        return $ take 1 $ a ++ b
    _ -> findMGUSubstitution t s

findMGUPair :: Type -> Type -> Maybe Substition
findMGUPair s t | s == t = Just []
                | otherwise = do
                    sub <- findMGUSubstitution s t
                    mgu <- findMGUPair (applySub sub s) (applySub sub t)
                    return $ sub ++ mgu

findMGU :: [Type] -> [Type] -> Maybe Substition
findMGU xs ys = findMGUPair xArrows yArrows where
    xArrows = f xs
    yArrows = f ys
    f = foldr1 Arrow

(<+>) :: Context -> (Ident, Type) -> Context
ctx <+> (ident, value) = (ident, value) : filter ((/=ident) . fst) ctx

deduceTypeProof :: Term -> Maybe TypeProof
deduceTypeProof term = evalStateT (rec term) $ [1..] >>= (`replicateM` ['a'..'z']) where
    rec :: Term -> StateT [String] Maybe TypeProof
    rec term = case term of 
        Var x -> do
            t <- TVar <$> freshTVarIdent
            return $ Assumption [(x, t)] term t
        Abs x s -> do
            let fv = freeVars s
            tps <- rec s
            let ctx = ctxFromTypeProof tps
            let finalCtx = filter ((/=x) . fst) ctx
            tx <- if x `elem` fv then lift $ lookup x ctx else TVar <$> freshTVarIdent
            return $ Abstraction finalCtx term (Arrow tx (typeFromTypeProof tps)) tps
        App s t -> do
            tps <- rec s
            tpt <- rec t
            let ts = typeFromTypeProof tps
            let tt = typeFromTypeProof tpt
            let ctxs = ctxFromTypeProof tps
            let ctxt = ctxFromTypeProof tpt
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
                    return $ Application (applySubCtx mgu $ onlySCtx ++ onlyTCtx ++ commonCtxS) term (applySub mgu q) (applySubTp mgu tps) (applySubTp mgu tpt)
                TVar name -> do
                    tret <- TVar <$> freshTVarIdent
                    mgu <- lift $ findMGU (ts : ctxAssignS) (Arrow tt tret : ctxAssignT)
                    return $ Application (applySubCtx mgu $ onlySCtx ++ onlyTCtx ++ commonCtxS) term (applySub mgu tret) (applySubTp mgu tps) (applySubTp mgu tpt)

deduceType :: Term -> Maybe Type
deduceType term = typeFromTypeProof <$> deduceTypeProof term
