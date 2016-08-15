{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SPI.Eval where

import Control.Arrow

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Data.Functor.Foldable (para)

import qualified Data.Map as M

import SPI.Expr
import SPI.Pretty
import SPI.Error
import qualified SPI.Value as Value

checkType :: forall m. (MonadError String m, MonadReader (Context, Maybe Expr) m, MonadState Int m) => Position -> Expr -> m Expr
checkType pos typ = do
  mrtyp <- asks snd
  case mrtyp of
    Nothing -> return ()
    Just rtyp -> do
      eq <- equal typ rtyp
      unless eq $
        typesDontMatchError pos typ rtyp
  return typ


inferType :: forall m. (MonadError String m, MonadReader (Context, Maybe Expr) m, MonadState Int m) => Expr -> m Expr
inferType expr = para alg expr
  where
    alg :: ExprF (Expr, m Expr) -> m Expr
    alg (Var pos x) = do
      mtype <- asks (lookupType x . fst)
      case mtype of
        Nothing -> unknownIdentifierError pos x
        Just t  -> checkType pos t

    alg (Universe pos n) = do
      return $ Fix $ Universe pos (succ n)

    alg (Pi pos x (texpr, t) (_, b)) = do
      k1 <- getUniverse =<< normalize =<< t
      k2 <- getUniverse =<< normalize =<< local (first $ extendCtx x texpr Nothing) b
      return $ Fix $ Universe pos (max k1 k2)

    alg (Lambda pos x Nothing (_, b)) = do
      mlamtype <- asks snd
      case mlamtype of
        Nothing ->
          throwError $ displayPos pos ++ ": ambiguous type for variable " ++ show x
        Just ty -> do
          (_, targ, tbody) <- getPi ty
          b' <- local (extendCtx x targ Nothing *** const (Just tbody)) b
          checkType pos (Fix $ Pi pos x targ b')

    alg (Lambda pos x (Just (texpr, t)) (_, b)) = do
      _ <- getUniverse =<< normalize =<< t
      b' <- local (first $ extendCtx x texpr Nothing) b
      return $ Fix $ Pi pos x texpr b'

    alg (App pos (_, f) (argexpr, a)) = do
      (x, targ, tbody) <- getPi =<< normalize =<< f
      targ' <- normalize =<< a
      eq <- equal targ targ'
      unless eq $
        typesDontMatchError pos targ targ'
      subst (M.singleton x argexpr) tbody

    alg (Annot pos (_, e) (texpr, t)) = do
      _ <- getUniverse =<< normalize =<< t
      inferred <- local (second (const (Just texpr))) e
      eq <- equal inferred texpr
      unless eq $
        typesDontMatchError pos inferred texpr
      return inferred

normalize :: forall m. (MonadError String m, MonadReader (Context, Maybe Expr) m, MonadState Int m) => Expr -> m Expr
normalize = Value.reify <=< Value.eval

equal :: (MonadError String m, MonadReader (Context, Maybe Expr) m, MonadState Int m) => Expr -> Expr -> m Bool
equal e1 e2 = do
  v1 <- Value.eval e1
  v2 <- Value.eval e2
  Value.equal v1 v2

----------------------------------------------------------------------
-- Errors

checkEqual :: (MonadError String m, MonadReader (Context, Maybe Expr) m, MonadState Int m) => Expr -> Expr -> m ()
checkEqual e1 e2 = do
  eq <- equal e1 e2
  unless eq $
    throwError $ displayPos (getPos e1) ++ ": terms are not equal:\n" ++ displayExpr e1 ++ "\n" ++ displayExpr e2
