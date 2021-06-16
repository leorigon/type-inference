module Lambda where

import Control.Monad
import Data.List (nubBy, union)
import Debug.Trace ()
import Env ( env )
import Type
    ( runTI,
      (@@),
      (-|>),
      unify,
      freshVar,
      Path(..),
      Assump(..),
      Subs(tv, apply),
      Literal(..),
      TI,
      SimpleType(TApp, TLit, TGen, TArr),
      Expr(Let, Var, Lit, App, Lam, If, Case),
      Type(..),
      Id )

errorMessage :: String -> error a
errorMessage i = error (
  "The variable" ++ i ++ ", doesn't appear at the current environment")

{-- tiContext g i = let (_ :>: t) = head (dropWhile (\(i' :>: _) -> i /= i' ) g)
  in t
  --}

tiContext g i = if l /= [] then tiInst t else errorMessage i
  where
    l = dropWhile (\(i' :>: _) -> i /= i') g
    (_ :>: t) = head l

{--
tiExpr g (Var i)  =
  case (lookup i g) of
    Nothing -> errorMessage i
    Just i  -> do
      let x = tiInst i
      return (x, [])--}

tiExpr g (Var i) = do
  x <- tiContext g i
  return (x, [])
tiExpr g (Lit i) = return (tiLit i)
tiExpr g (App e e') = do
  (t, s1) <- tiExpr g e
  (t', s2) <- tiExpr (apply s1 g) e'
  b <- freshVar
  let s3 = unify (apply s2 t) (t' -|> b)
  return (apply s3 b, s3 @@ s2 @@ s1)
tiExpr g (Lam i e) = do
  b <- freshVar
  --traceM $ "b = " ++ show b
  (t, s) <- tiExpr (g `assumpConcat` [i :>: Forall b]) e
  --traceM $ "t, s = " ++ show t ++ "," ++ show s
  return (apply s (b -|> t), s)
tiExpr g (If cond e e') = do
  (t1, s1) <- tiExpr g cond
  let s4 = unify t1 (TLit Bool)
      tx = apply s4 g
  (t2, s2) <- tiExpr (apply s1 tx) e
  (t3, s3) <- tiExpr (apply s2 tx) e'
  let s5 = unify t2 t3
  return (apply s5 t2, s5 @@ s4 @@ s3 @@ s2 @@ s1)
tiExpr g (Case e legs) = do
  (tE, gE) <- tiExpr g e
  let solver (t1, gE) (pat, e1) = do
        (tP, gP) <- tiPath g pat
        --traceM $ "tP e gP = " ++ show tP ++ ", " ++  show gP
        let sx = unify tE tP
        --traceM $ "sx = " ++ show sx
        --let mg = sPconcat (apply sx g) (apply sx gP)
        --traceM $ "mg = " ++ show mg
        (tE2, gE2) <- tiExpr (assumpConcat (apply sx g) (apply sx gP)) e1
        let vr2 = unify t1 tE2
        --traceM $ "vr2 = " ++ show vr2
        return (tE2, vr2 @@ sx @@ gE2)
  foldM solver (tE, gE) legs
tiExpr g (Let (i, e) ex) = do
  (tE, s1) <- tiExpr g e
  let sx = tiGen (tv tE) tE
  (tEx, s2) <- tiExpr (apply s1 (g `assumpConcat` [i :>: sx])) ex
  return (apply (s2 @@ s1) tEx, s2 @@ s1)

tiPath :: [Assump] -> Path -> TI (SimpleType, [Assump])
tiPath g (PLit l) = return (tiLit l)
tiPath g (PVar x) = do
  b <- freshVar
  return (b, g `assumpConcat` [x :>: Forall b])
tiPath g (PCon i patt) = do
  (t1, s1) <- tiPats g patt
  b <- freshVar
  --traceM $ "s1 = " ++ show s1
  tx <- tiContext (assumpConcat s1 g) i
  --traceM $ "tx = " ++ show tx
  let sx = unify tx (foldr (-|>) b t1)
  return (apply sx b, assumpConcat s1 g)

tiPats :: [Assump] -> [Path] -> TI ([SimpleType], [Assump])
tiPats g pats = do
  ts <- mapM (tiPath g) pats
  --traceM $ "ts = " ++ show ts
  let t1 = map fst ts
      s1 = map snd ts
  --traceM $ "t1 = " ++ show t1
  --traceM $ "s1 = " ++ show s1
  return (t1, foldrUnion s1)

tiLit :: Literal -> (SimpleType, [a])
tiLit (LInt n) = (TLit Int, [])
tiLit (LBool b) = (TLit Bool, [])

tiInst :: Type -> TI SimpleType
tiInst (Forall t) = do
  st <- mapM (const freshVar) [1 .. (getMaxValue t)]
  return (instantiate st t)

tiGen :: Foldable t => t Id -> SimpleType -> Type
tiGen vs qt = Forall (apply s qt)
  where
    vs' = [v | v <- tv qt, v `elem` vs]
    s = zip vs' (map TGen [1 ..])

assumpConcat :: [Assump] -> [Assump] -> [Assump]
assumpConcat first last = nubBy condition (first ++ last)

condition :: Assump -> Assump -> Bool
condition (var1 :>: _) (var2 :>: _) = var1 == var2

foldrUnion :: [[Assump]] -> [Assump]
foldrUnion = foldr union []

instantiate :: [SimpleType] -> SimpleType -> SimpleType
instantiate g (TGen l) = g !! l
instantiate g (TArr l r) = TArr (instantiate g l) (instantiate g r)
instantiate g (TApp l r) = TApp (instantiate g l) (instantiate g r)
instantiate _ l = l

getMaxValue :: SimpleType -> Int
getMaxValue (TGen x) = x
getMaxValue (TArr l r) = max (getMaxValue l) (getMaxValue r)
getMaxValue (TApp l r) = max (getMaxValue l) (getMaxValue r)
getMaxValue _ = 0

getEnv :: [Assump]
getEnv = map quant env

quant :: (Id, SimpleType) -> Assump
quant (i, t) = i :>: tiGen (tv t) t

infer :: Expr -> (SimpleType, [(Id, SimpleType)])
infer e = runTI (tiExpr getEnv e)
