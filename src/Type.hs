{-# LANGUAGE TupleSections #-}

module Type where

import Data.List (nub, union)

newtype TI a = TI (Index -> (a, Index))

newtype Type = Forall SimpleType deriving (Eq, Show)

type Index = Int

type Id = String

type Substitution = [(Id, SimpleType)]

data Assump = Id :>: Type deriving (Eq, Show)

data Expr
  = Var Id
  | App Expr Expr
  | Lam Id Expr
  | Lit Literal
  | If Expr Expr Expr
  | Case Expr [(Path, Expr)]
  | Let (Id, Expr) Expr
  | DataConstructor (Id, Id) Expr
  deriving (Eq, Show)

data SimpleType
  = TVar Id
  | TLit Literal
  | TCon Id
  | TApp SimpleType SimpleType
  | TArr SimpleType SimpleType
  | TGen Int
  deriving (Eq)

data Path
  = PVar Id
  | PLit Literal
  | PCon Id [Path]
  deriving (Show, Eq)

data Literal
  = LInt Int
  | LBool Bool
  | Int
  | Bool
  deriving (Show, Eq)

instance Show SimpleType where
  show (TGen x) = show x
  show (TCon x) = x
  show (TLit x) = show x
  show (TVar i) = i
  show (TArr (TVar i) t) = i ++ " -> " ++ show t
  show (TArr (TCon x) t) = show x ++ " -> " ++ show t
  show (TArr (TLit x) t) = show x ++ " -> " ++ show t
  show (TArr t t') = "(" ++ show t ++ ")" ++ " -> " ++ show t'
  show (TApp t t') = show t ++ " " ++ show t'

instance Functor TI where
  fmap f (TI m) = TI (\e -> let (a, e') = m e in (f a, e'))

instance Applicative TI where
  pure a = TI (a,)
  TI fs <*> TI vs =
    TI (\e -> let (f, e') = fs e; (a, e'') = vs e' in (f a, e''))

instance Monad TI where
  return x = TI (x,)
  TI m >>= f = TI (\e -> let (a, e') = m e; TI fa = f a in fa e')

freshVar :: TI SimpleType
freshVar = TI (\e -> let v = "t" ++ show e in (TVar v, e + 1))

runTI :: TI a -> a
runTI (TI m) = let (t, _) = m 0 in t

(-|>) :: SimpleType -> SimpleType -> SimpleType
t -|> t' = TArr t t'

infixr 4 @@

(@@) :: Substitution -> Substitution -> Substitution
s1 @@ s2 = [(u, apply s1 t) | (u, t) <- s2] ++ s1

class Subs t where
  apply :: Substitution -> t -> t
  tv :: t -> [Id]

instance Subs SimpleType where
  apply s (TVar u) = case lookup u s of
    Just t -> t
    Nothing -> TVar u
  apply s (TCon u) = case lookup u s of
    Just t -> t
    Nothing -> TCon u
  apply s (TArr l r) = TArr (apply s l) (apply s r)
  apply s (TApp l r) = TApp (apply s l) (apply s r)
  apply s (TLit l) = TLit l
  apply s (TGen l) = TGen l

  tv (TVar u) = [u]
  tv (TArr l r) = tv l `union` tv r
  tv (TApp l r) = tv l `union` tv r
  tv (TCon u) = []
  tv (TLit l) = []

instance Subs Assump where
  apply s (i :>: t) = i :>: apply s t
  tv (i :>: t) = tv t

instance Subs a => Subs [a] where
  apply s = map (apply s)
  tv = nub . concatMap tv

instance Subs Type where
  apply s (Forall qt) = Forall (apply s qt)
  tv (Forall qt) = tv qt

unify :: SimpleType -> SimpleType -> Substitution
unify t t' = case mgu (t, t') of
  Nothing ->
    error
      ("unification: trying to unify\n" ++ show t ++ "\nand\n" ++ show t')
  Just s -> s

mgu :: (SimpleType, SimpleType) -> Maybe Substitution
mgu (TArr l r, TArr l' r') = do
  s1 <- mgu (l, l')
  s2 <- mgu (apply s1 r, apply s1 r')
  return (s2 @@ s1)
mgu (TApp l r, TApp l' r') = do
  s1 <- mgu (l, l')
  s2 <- mgu (apply s1 r, apply s1 r')
  return (s2 @@ s1)
mgu (t, TVar u) = varBind u t
mgu (TVar u, t) = varBind u t
mgu (TLit u, TLit t) = if u == t then Just [] else Nothing
mgu (TCon u, TCon t) = if u == t then Just [] else Nothing
mgu (TCon u, t) = varBind u t
mgu (t, TCon u) = varBind u t
mgu (u, t) = if u == t then Just [] else Nothing

varBind :: Id -> SimpleType -> Maybe Substitution
varBind u t
  | t == TVar u = Just []
  | t == TCon u = Just []
  | u `elem` tv t = Nothing
  | otherwise = Just [(u, t)]