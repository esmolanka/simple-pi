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
normalize =
  Value.reify <=< Value.eval

equal :: (MonadError String m, MonadReader Context m, MonadState Int m) => Expr -> Expr -> m Bool
equal e1 e2 = do
  v1 <- Value.eval e1
  v2 <- Value.eval e2
  Value.equal v1 v2

----------------------------------------------------------------------
-- Errors

checkEqual :: (MonadError String m, MonadReader Context m, MonadState Int m) => Expr -> Expr -> m ()
checkEqual e1 e2 = do
  eq <- equal e1 e2
  unless eq $
    throwError $ displayPos (getPos e1) ++ ": terms are not equal:\n" ++ displayExpr e1 ++ "\n" ++ displayExpr e2
