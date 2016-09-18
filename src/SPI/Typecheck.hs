{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SPI.Typecheck where

import Control.Arrow ((&&&))

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Data.Functor.Foldable (para)
import qualified Data.Map as M

import SPI.Env
import SPI.Error
import SPI.Expr
import qualified SPI.Value as Value

-- import Debug.Trace

import Language.SimplePi.Types (Position (..), dummyPos)

checkType' :: forall m. (MonadError String m, MonadReader Env m, MonadState Int m) => Position -> TypeExpectation -> Expr -> m Expr
checkType' pos ann typ = do
  case ann of
    Any -> return ()
    LamArg argt' _ann' -> do
      (_, argt, _bodyt) <- getPi pos "checking type against lambda" =<< normalize typ
      eq <- equal argt argt'
      unless eq $
        typesDontMatchError pos argt argt'
    Exactly rtyp -> do
      eq <- equal typ rtyp
      unless eq $
        typesDontMatchError pos typ rtyp
  return typ

checkType :: forall m. (MonadError String m, MonadReader Env m, MonadState Int m) => Position -> Expr -> m Expr
checkType pos typ = do
  ann <- asks annotation
  checkType' pos ann typ

inferTypeAnnot :: forall m. (MonadError String m, MonadReader Env m, MonadState Int m) => TypeExpectation -> Expr -> m Expr
inferTypeAnnot annot expr =
  local (\env -> env { annotation = annot }) (inferType expr)

inferType :: forall m. (MonadError String m, MonadReader Env m, MonadState Int m) => Expr -> m Expr
inferType expr = do
  subst M.empty expr >>= para (alg . traceTC)
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
      k1 <- getUniverse =<< normalize =<< t ?: Any
      k2 <- getUniverse =<< normalize =<< inContext x texpr b ?: Any
      checkType pos $ Fix $ Universe pos (max k1 k2)

    alg (Lambda pos arg marg (_, b)) = do
      ann <- getAnnot
      (margty, bodyty'') <- case ann of
        Any -> return (Nothing, Any)
        LamArg ty ann -> return (Just ty, ann)
        Exactly ty -> do
          (var, argty, bodyty) <- getPi dummyPos "expecting Pi" =<< normalize ty
          ann <- Exactly <$> subst (M.singleton var (Fix $ Var pos arg)) bodyty
          return (Just argty, ann)

      argty <- case (margty, fmap fst marg) of
        (Nothing, Nothing) -> throwError $ displayPos pos ++ ": cannot infer type for variable " ++ pp (Fix $ Var pos arg)
        (Just ty, Just ty') -> do
          eq <- equal ty ty'
          unless eq $
            typesDontMatchError pos ty ty'
          return ty
        (Just ty, Nothing) -> return ty
        (Nothing, Just ty) -> return ty

      bodyty <- inContext arg argty $ b ?: bodyty''
      checkType pos (Fix $ Pi pos arg argty bodyty)

    alg (App pos (_, f) (argexpr, a)) = do
      (x, tbody) <-
        (do
            (x, targ, tbody) <- getPi pos "application" =<< normalize =<< f ?: Any
            targ' <- normalize =<< a ?: Exactly targ
            eq <- equal targ targ'
            unless eq $
              typesDontMatchError pos targ targ'
            return (x, tbody)
        ) `catchError`
        (\_ -> do
            t <- normalize =<< a ?: Any
            ann <- getAnnot
            (x, _, tbody) <- getPi pos "application" =<< normalize =<< f ?: LamArg t ann
            return (x, tbody)
        )

      checkType pos =<< subst (M.singleton x argexpr) tbody

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
    typesDontMatchError (getPos e1) e1 e2

----------------------------------------------------------------------
-- Trace

trace :: String -> a -> a
trace = flip const

traceTC :: forall m. (MonadError String m, MonadReader Env m, MonadState Int m) => ExprF (Expr, m Expr) -> ExprF (Expr, m Expr)
traceTC e =
  let trc :: (Expr, m Expr) -> m Expr
      trc (expr, m) = do
        annot <- asks annotation
        let txt = "In: " ++ pp (Fix $ fmap fst e) ++ "\n  " ++
                  pp expr ++ " <: " ++ ppAnnot annot
        trace txt $ return ()
        r <- m
        let txt' = "In: " ++ pp (Fix $ fmap fst e) ++ "\n  " ++
                   pp expr ++ " :> " ++ pp r
        trace txt' $ return r
  in fmap (fst &&& trc) e

ppAnnot :: TypeExpectation -> String
ppAnnot x = case x of
  Any -> "⋆"
  LamArg a ann -> pp a ++ " → " ++ ppAnnot ann
  Exactly b -> pp b
