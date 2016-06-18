{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Expr where

import Control.Monad.State.Strict
import Control.Monad.Except

import qualified Data.Map as M
import Data.Map (Map)
import GHC.Generics (Generic)

data Variable
  = VarStr String
  | GenSym String Int
  | Dummy
    deriving (Show, Eq, Ord, Generic)

data Abstraction = Abstraction Variable Expr Expr
  deriving (Show, Eq, Ord, Generic)

data Expr
  = Var Variable
  | Universe Int
  | Pi Abstraction
  | Lambda Abstraction
  | App Expr Expr
    deriving (Show, Eq, Ord, Generic)

refresh :: (MonadState Int m) => Variable -> m Variable
refresh (VarStr x)   = modify succ >> GenSym x   <$> get
refresh (GenSym x _) = modify succ >> GenSym x   <$> get
refresh Dummy        = modify succ >> GenSym "_" <$> get

subst :: (MonadState Int m) => Map Variable Expr -> Expr -> m Expr
subst env expr =
  case expr of
    Var x -> return $
      case M.lookup x env of
        Just e -> e
        Nothing -> Var x
    Universe k ->
      return $ Universe k
    Pi a ->
      Pi <$> substAbs env a
    Lambda a ->
      Lambda <$> substAbs env a
    App e1 e2 ->
      App <$> subst env e1 <*> subst env e2

substAbs :: (MonadState Int m) => Map Variable Expr -> Abstraction -> m Abstraction
substAbs env (Abstraction x t e) = do
  x' <- refresh x
  Abstraction
    <$> pure x'
    <*> subst env t
    <*> subst (M.insert x (Var x') env) e

newtype Context = Context (Map Variable (Expr, Maybe Expr))
  deriving (Show, Eq, Ord)

lookupType :: Variable -> Context -> Maybe Expr
lookupType v (Context c) = fst <$> M.lookup v c

lookupValue :: Variable -> Context -> Maybe (Maybe Expr)
lookupValue v (Context c) = snd <$> M.lookup v c

extend :: Variable -> Expr -> Maybe Expr -> Context -> Context
extend v t b (Context c) = Context $ M.insert v (t, b) c

fresh :: Context
fresh = Context M.empty

inferType :: (MonadError String m, MonadState Int m) => Context -> Expr {- value -} -> m Expr {- type -}
inferType ctx expr =
  case expr of
    Var x ->
      case lookupType x ctx of
        Just ty -> return ty
        Nothing -> throwError $ "Unknown identifier: " ++ show x
    Universe k -> return $ Universe (succ k)
    Pi (Abstraction x t1 t2) -> do
      k1 <- inferUniverse ctx t1
      k2 <- inferUniverse (extend x t1 Nothing ctx) t2
      return $ Universe (max k1 k2)
    Lambda (Abstraction x t e) -> do
      _ <- inferUniverse ctx t
      te <- inferType (extend x t Nothing ctx) e
      return $ Pi (Abstraction x t te)
    App e1 e2 -> do
      Abstraction x s t <- inferPi ctx e1
      te <- inferType ctx e2
      checkEqual ctx s te
      subst (M.singleton x e2) t

inferUniverse :: (MonadError String m, MonadState Int m) => Context -> Expr -> m Int
inferUniverse ctx t = do
  u <- inferType ctx t
  n <- normalize ctx u
  case n of
    Universe k -> return k
    other -> throwError $ "Type expected: " ++ show other

inferPi :: (MonadError String m, MonadState Int m) => Context -> Expr -> m Abstraction
inferPi ctx e = do
  t <- inferType ctx e
  n <- normalize ctx t
  case n of
    Pi a -> return a
    other -> throwError $ "Function expected: " ++ show other

checkEqual :: (MonadError String m, MonadState Int m) => Context -> Expr -> Expr -> m ()
checkEqual ctx e1 e2 = do
  eq <- equal ctx e1 e2
  if eq
    then return ()
    else throwError $ "Expressions are not equal: " ++ show e1 ++ "\n\n" ++ show e2

normalize :: (MonadError String m, MonadState Int m) => Context -> Expr -> m Expr
normalize ctx expr =
  case expr of
    Var x ->
      case lookupValue x ctx of
        Just Nothing -> return (Var x)
        Just (Just e) -> normalize ctx e
        Nothing -> throwError $ "Unknown identifier: " ++ show x
    App e1 e2 -> do
      e2' <- normalize ctx e2
      e1' <- normalize ctx e1
      case e1' of
        Lambda (Abstraction x _ e1'') -> normalize ctx =<< subst (M.singleton x e2) e1''
        _ -> return $ App e1' e2'
    Universe k -> return $ Universe k
    Pi a -> Pi <$> normalizeAbs ctx a
    Lambda a -> Lambda <$> normalizeAbs ctx a
  where
    normalizeAbs :: (MonadError String m, MonadState Int m) => Context -> Abstraction -> m Abstraction
    normalizeAbs ctx (Abstraction x t e) = do
      t' <- normalize ctx t
      Abstraction x t' <$> normalize (extend x t Nothing ctx) e

equal :: (MonadError String m, MonadState Int m) => Context -> Expr -> Expr -> m Bool
equal ctx e1 e2 = do
  e1' <- normalize ctx e1
  e2' <- normalize ctx e2
  equalExpr e1' e2'
  where
    equalExpr e1 e2 = case (e1, e2) of
      (Var x1, Var x2) -> return $ x1 == x2
      (App e11 e12, App e21 e22) -> (&&) <$> equalExpr e11 e21 <*> equalExpr e12 e22
      (Universe k, Universe k') -> return $ k == k'
      (Pi a, Pi b) -> equalAbs a b
      (Lambda a, Lambda b) -> equalAbs a b
      _ -> return False

    equalAbs (Abstraction x t1 e1) (Abstraction y t2 e2) = do
      ts <- equalExpr t1 t2
      es <- equalExpr e1 =<< subst (M.singleton y (Var x)) e2
      return (ts && es)
