{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SPI.Eval where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Data.Functor.Foldable (para)

import qualified Data.Map as M

import SPI.Expr
import SPI.Pretty
import SPI.Error
import qualified SPI.Value as Value

import SPI.Env

checkType :: forall m. (MonadError String m, MonadReader Env m, MonadState Int m) => Position -> Expr -> m Expr
checkType pos typ = do
  ann <- asks annotation
  case ann of
    Any -> return ()
    LamArg argt' -> do
      (_, argt, _) <- getPi pos "checking type against lambda" =<< normalize typ
      eq <- equal argt argt'
      unless eq $
        typesDontMatchError pos argt argt'
    Exactly rtyp -> do
      eq <- equal typ rtyp
      unless eq $
        typesDontMatchError pos typ rtyp
  return typ

expectPi :: forall m. (MonadError String m, MonadReader Env m, MonadState Int m) => m (Maybe Expr, Maybe Expr)
expectPi = do
  ann <- asks annotation
  case ann of
    Any -> return (Nothing, Nothing)
    LamArg ty -> return (Just ty, Nothing)
    Exactly ty -> do
      (_, arg, body) <- getPi dummyPos "expecting Pi" =<< normalize ty
      return (Just arg, Just body)

checkArgType :: forall m. (MonadError String m, MonadReader Env m, MonadState Int m) => Position -> Variable -> Maybe Expr -> m Expr
checkArgType pos var mty = do
  (mty', _) <- expectPi
  case (mty, mty') of
    (Nothing, Nothing) ->
      throwError $ displayPos pos ++ ": cannot infer type for variable " ++ show var
    (Just t, Nothing) -> return t
    (Nothing, Just t) -> return t
    (Just t,  Just t') -> do
      eq <- equal t t'
      unless eq $
        typesDontMatchError pos t t'
      return t

inferType :: forall m. (MonadError String m, MonadReader Env m, MonadState Int m) => Expr -> m Expr
inferType expr = para alg expr
  where
    alg :: ExprF (Expr, m Expr) -> m Expr
    alg (Var pos x) = do
      mtype <- asks (lookupType x . gamma)
      case mtype of
        Nothing -> unknownIdentifierError pos x
        Just t  -> checkType pos t

    alg (Universe pos n) = do
      checkType pos $ Fix $ Universe pos (succ n)

    alg (Pi pos x (texpr, t) (_, b)) = do
      k1 <- getUniverse =<< normalize =<< eraseAnnot t
      k2 <- getUniverse =<< normalize =<< eraseAnnot (inContext x texpr b)
      checkType pos $ Fix $ Universe pos (max k1 k2)

    alg (Lambda pos arg marg (_, b)) = do
      argty   <- checkArgType pos arg (fmap fst marg)
      bodyty' <- fmap snd expectPi
      bodyty  <- inContext arg argty $ maybe eraseAnnot withAnnot bodyty' $ b
      checkType pos (Fix $ Pi pos arg argty bodyty)

    alg (App pos (_, f) (argexpr, a)) = do
      targ' <- normalize =<< eraseAnnot a
      (x, targ, tbody) <- getPi pos "application" =<< normalize =<< withLamArgAnnot targ' f
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

normalize :: forall m. (MonadError String m, MonadReader Env m, MonadState Int m) => Expr -> m Expr
normalize = Value.reify <=< Value.eval

equal :: (MonadError String m, MonadReader Env m, MonadState Int m) => Expr -> Expr -> m Bool
equal e1 e2 = do
  v1 <- Value.eval e1
  v2 <- Value.eval e2
  Value.equal v1 v2

----------------------------------------------------------------------
-- Errors

checkEqual :: (MonadError String m, MonadReader Env m, MonadState Int m) => Expr -> Expr -> m ()
checkEqual e1 e2 = do
  eq <- equal e1 e2
  unless eq $
    throwError $ displayPos (getPos e1) ++ ": terms are not equal:\n" ++ displayExpr e1 ++ "\n" ++ displayExpr e2
