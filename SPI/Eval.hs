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

eraseAnnot :: forall a m. (MonadError String m, MonadReader (Context, Maybe Expr) m, MonadState Int m) => m a -> m a
eraseAnnot = local (second (const Nothing))

withAnnot :: forall a m. (MonadError String m, MonadReader (Context, Maybe Expr) m, MonadState Int m) => Expr -> m a -> m a
withAnnot ty = local (second (const (Just ty)))

inContext :: forall a m. (MonadError String m, MonadReader (Context, Maybe Expr) m, MonadState Int m) => Variable -> Expr -> m a -> m a
inContext var typ = local (first $ extendCtx var typ Nothing)

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
      checkType pos $ Fix $ Universe pos (succ n)

    alg (Pi pos x (texpr, t) (_, b)) = do
      k1 <- getUniverse =<< normalize =<< eraseAnnot t
      k2 <- getUniverse =<< normalize =<< inContext x texpr b
      checkType pos $ Fix $ Universe pos (max k1 k2)

    alg (Lambda pos x Nothing (_, b)) = do
      mlamtype <- asks snd
      case mlamtype of
        Nothing ->
          throwError $ displayPos pos ++ ": ambiguous type for variable " ++ show x
        Just ty -> do
          (_, targ, tbody) <- getPi ty
          b' <- inContext x targ $ withAnnot tbody b
          checkType pos (Fix $ Pi pos x targ b')

    alg (Lambda pos x (Just (texpr, t)) (_, b)) = do
      mlamtype <- asks snd
      b' <- case mlamtype of
        Just pi -> do
          (_, targ, tbody) <- getPi =<< normalize pi
          _ <- getUniverse =<< normalize =<< withAnnot targ t
          inContext x texpr $ withAnnot tbody b
        Nothing -> do
          _ <- getUniverse =<< normalize =<< t
          inContext x texpr $ eraseAnnot b
      checkType pos $ Fix $ Pi pos x texpr b'

    alg (App pos (_, f) (argexpr, a)) = do
      (x, targ, tbody) <- getPi =<< normalize =<< eraseAnnot f
      targ' <- normalize =<< eraseAnnot a
      eq <- equal targ targ'
      unless eq $
        typesDontMatchError pos targ targ'
      checkType pos =<< subst (M.singleton x argexpr) tbody

    alg (Annot pos (_, e) (texpr, t)) = do
      _ <- getUniverse =<< normalize =<< t
      inferred <- withAnnot texpr e
      eq <- equal inferred texpr
      unless eq $
        typesDontMatchError pos inferred texpr
      checkType pos inferred

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
