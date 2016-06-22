{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SPI.Eval where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Data.Functor.Foldable (cata, para)

import qualified Data.Map as M
import Data.Map (Map)

import SPI.Expr
import SPI.Pretty

subst :: forall m. (MonadError String m, MonadState Int m) => Map Variable Expr -> Expr -> m Expr
subst env expr = runReaderT (cata alg expr) env
  where
    alg :: ExprF (ReaderT (Map Variable Expr) m Expr)
        -> ReaderT (Map Variable Expr) m Expr
    alg (Var pos x) = do
      expr' <- asks (M.lookup x)
      case expr' of
        Nothing -> return $ Fix $ Var pos x
        Just expr -> return expr
    alg (Universe pos k) =
      return $ Fix $ Universe pos k
    alg (Pi pos x ty body) = do
      x' <- refresh x
      ty' <- ty
      body' <- local (M.insert x (Fix $ Var pos x')) body
      return $ Fix $ Pi pos x' ty' body'
    alg (Lambda pos x ty body) = do
      x' <- refresh x
      ty' <- ty
      body' <- local (M.insert x (Fix $ Var pos x')) body
      return $ Fix $ Lambda pos x' ty' body'
    alg (App pos e1 e2) = do
      e1' <- e1
      e2' <- e2
      return $ Fix $ App pos e1' e2'

    refresh :: forall m. (MonadError String m, MonadState Int m) => Variable -> m Variable
    refresh (VarStr x)   = modify succ >> GenSym x   <$> get
    refresh (GenSym x _) = modify succ >> GenSym x   <$> get
    refresh Dummy        = modify succ >> GenSym "_" <$> get

newtype Context = Context (Map Variable (Expr, Maybe Expr))
  deriving (Show, Eq, Ord)

lookupType :: Variable -> Context -> Maybe Expr
lookupType v (Context c) = fst <$> M.lookup v c

lookupValue :: Variable -> Context -> Maybe (Maybe Expr)
lookupValue v (Context c) = snd <$> M.lookup v c

extendCtx :: Variable -> Expr -> Maybe Expr -> Context -> Context
extendCtx v t b (Context c) = Context $ M.insert v (t, b) c

freshCtx :: Context
freshCtx = Context $ M.fromList
  [ (VarStr "*", (Fix $ Universe dummyPos 1, Just $ Fix $ Universe dummyPos 0)) ]

inferType :: forall m. (MonadError String m, MonadReader Context m, MonadState Int m) => Expr -> m Expr
inferType expr = para alg expr
  where
    alg :: ExprF (Expr, m Expr) -> m Expr
    alg (Var pos x) =
      asks (lookupType x) >>= maybe (unknownIdentifierError pos x) return
    alg (Universe pos n) = return $ Fix $ Universe pos (succ n)
    alg (Pi pos x (texpr, t) (_, b)) = do
      k1 <- getUniverse =<< normalize =<< t
      k2 <- getUniverse =<< normalize =<< local (extendCtx x texpr Nothing) b
      return $ Fix $ Universe pos (max k1 k2)
    alg (Lambda pos x (texpr, t) (_, b)) = do
      _ <- getUniverse =<< normalize =<< t
      b' <- local (extendCtx x texpr Nothing) b
      return $ Fix $ Pi pos x texpr b'
    alg (App pos (_, f) (argexpr, a)) = do
      (x, targ, tbody) <- getPi =<< normalize =<< f
      targ' <- normalize =<< a
      eq <- equal targ targ'
      unless eq $
        typesDontMatchError pos targ targ'
      subst (M.singleton x argexpr) tbody

normalize :: forall m. (MonadError String m, MonadReader Context m, MonadState Int m) => Expr -> m Expr
normalize = cata alg
  where
    alg :: ExprF (m Expr) -> m Expr
    alg (Var pos x) = do
      var <- asks (lookupValue x)
      case var of
        Nothing -> unknownIdentifierError pos x
        Just Nothing -> return $ Fix $ Var pos x
        Just (Just expr) -> normalize expr
    alg (Universe pos n) = return $ Fix $ Universe pos n
    alg (Pi pos x t b) = do
      t' <- t
      b' <- local (extendCtx x t' Nothing) b
      return $ Fix $ Pi pos x t' b'
    alg (Lambda pos x t b) = do
      t' <- t
      b' <- local (extendCtx x t' Nothing) b
      return $ Fix $ Lambda pos x t' b'
    alg (App pos f a) = do
      f' <- f
      a' <- a
      case f' of
        Fix (Lambda _pos x _ fbody) ->
          subst (M.singleton x a') fbody >>= normalize
        _other -> return $ Fix $ App pos f' a'

equal :: (MonadError String m, MonadReader Context m, MonadState Int m) => Expr -> Expr -> m Bool
equal e1 e2 = do
  e1' <- normalize e1
  e2' <- normalize e2
  equalExpr e1' e2'
  where
    equalExpr e1 e2 = case (unFix e1, unFix e2) of
      (Var _ x1, Var _ x2) -> return $ x1 == x2
      (Universe _ n1, Universe _ n2) -> return $ n1 == n2
      (Pi pos x1 t1 b1, Pi _ x2 t2 b2) -> equalAbs pos (x1, t1, b1) (x2, t2, b2)
      (Lambda pos x1 t1 b1, Lambda _ x2 t2 b2) -> equalAbs pos (x1, t1, b1) (x2, t2, b2)
      (App _ f1 a1, App _ f2 a2) -> (&&) <$> equalExpr f1 f2 <*> equalExpr a1 a2
      _ -> return False

    equalAbs pos (x1, t1, b1) (x2, t2, b2) = do
      ts <- equalExpr t1 t2
      bs <- equalExpr b1 =<< subst (M.singleton x2 (Fix $ Var pos x1)) b2
      return (ts && bs)

----------------------------------------------------------------------
-- Errors

unknownIdentifierError :: (MonadError String m) => Position -> Variable -> m a
unknownIdentifierError pos var =
  throwError $ displayPos pos ++ ": unknown identifier: " ++ displayExpr (Fix $ Var dummyPos var)

typesDontMatchError :: (MonadError String m) => Position -> Expr -> Expr -> m a
typesDontMatchError pos t1 t2 =
  throwError $ displayPos pos ++ ": types do not match:\n" ++ displayExpr t1 ++ "\n" ++ displayExpr t2

getUniverse :: (MonadError String m) => Expr -> m Int
getUniverse (Fix (Universe _pos x)) = return x
getUniverse other = throwError $ displayPos (getPos other) ++ ": type expected:\n" ++ displayExpr other

getPi  :: (MonadError String m) => Expr -> m (Variable, Expr, Expr)
getPi (Fix (Pi _pos x t1 t2)) = return (x, t1, t2)
getPi other = throwError $ displayPos (getPos other) ++ ": function expected:\n" ++ displayExpr other

checkEqual :: (MonadError String m, MonadReader Context m, MonadState Int m) => Expr -> Expr -> m ()
checkEqual e1 e2 = do
  eq <- equal e1 e2
  unless eq $
    throwError $ displayPos (getPos e1) ++ ": terms are not equal:\n" ++ displayExpr e1 ++ "\n" ++ displayExpr e2
