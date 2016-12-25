{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SimpTT.Eval
  ( whnf
  , normalize
  , equal
  , inferType
  , Ctx
  , Err
  , runEval
  ) where

import Control.Monad.Reader
import Control.Monad.Except

import Data.Functor.Foldable (Fix (..), cata, para)
import Data.Text.Lazy (unpack)

import Language.SimplePi.Types (Position)
import Language.SimplePi.Pretty (ppExpr)

import Text.PrettyPrint.Leijen.Text (displayT, renderPretty, pretty)

import SimpTT.Sugar
import qualified SimpTT.Context as Ctx
import qualified SimpTT.Error as Error
import SimpTT.Expr

type Ctx  = Ctx.Context Expr
type Err  = Error.TTError Ctx.Context Expr
type Type = Expr

whnf :: forall m. (MonadError Err m, MonadReader Ctx m) => Expr -> m Expr
whnf = para alg
  where
    alg :: ExprF (Expr, m Expr) -> m Expr
    alg (Var pos x n) = evalVar pos x n whnf
    alg (App pos f a) = betaReduce pos (snd f) (return (fst a)) whnf
    alg other         = return $ Fix $ fmap fst other

normalize :: forall m. (MonadError Err m, MonadReader Ctx m) => Expr -> m Expr
normalize = cata alg
  where
    alg :: ExprF (m Expr) -> m Expr
    alg (Var pos x n)      = evalVar pos x n normalize
    alg (Lambda pos x t b) = etaReduce pos x t b
    alg (Pi pos x k t)     = do
      k' <- k
      t' <- inContext x k' t
      return (Fix (Pi pos x k' t'))
    alg (App pos f a)      = betaReduce pos f a normalize
    alg (Universe pos u)   = return (Fix (Universe pos u))

evalVar
  :: forall m. (MonadError Err m, MonadReader Ctx m) =>
     Position
  -> Variable
  -> Integer
  -> (Expr -> m Expr)
  -> m Expr
evalVar pos x n cont = do
  var <- asks (fmap Ctx.entryTerm . Ctx.lookup x n)
  ctx <- ask
  case var of
    Nothing -> throwError $ Error.VariableNotFound pos (Fix (Var pos x n)) ctx
    Just Nothing -> return (Fix (Var pos x n))
    Just (Just e) -> cont e

etaReduce
  :: forall m. (MonadError Err m, MonadReader Ctx m) =>
     Position
  -> Variable
  -> m Expr
  -> m Expr
  -> m Expr
etaReduce pos x t b = do
  t' <- t
  b' <- inContext x t' b
  case b' of
    Fix (App _ f' (Fix (Var _ x' 0)))
      | x == x' && not (free x 0 f') -> return f'
    _ -> return (Fix (Lambda pos x t' b'))

betaReduce
  :: forall m. (MonadError Err m, MonadReader Ctx m) =>
     Position
  -> m Expr
  -> m Expr
  -> (Expr -> m Expr)
  -> m Expr
betaReduce pos f a cont = do
  f' <- f
  a' <- a
  case f' of
    Fix (Lambda _ x _ b') -> do
      cont $ shift (-1) x $ subst x 0 (shift 1 x a') b'
    other ->
      return (Fix $ App pos other a')

----------------------------------------------------------------------
-- Equality

equal :: forall m. (MonadError Err m, MonadReader Ctx m) => Expr -> Expr -> m Bool
equal a b = areEqual <$> normalize a <*> normalize b

checkEqualTypes :: forall m. (MonadError Err m, MonadReader Ctx m) => Position -> String -> Expr -> Type -> Type -> m ()
checkEqualTypes pos msg term ta tb = do
  equal ta tb >>= \yes ->
    unless yes $
      throwError $
        Error.TypeMismatch pos msg term ta tb

----------------------------------------------------------------------
-- Type check

inferType :: forall m. (MonadError Err m, MonadReader Ctx m) => Expr -> m Type
inferType = para alg
  where
    alg :: ExprF (Expr, m Type) -> m Type
    alg (Var pos x n) = do
      mty <- asks (fmap Ctx.entryType . Ctx.lookup x n)
      ctx <- ask
      case mty of
        Nothing ->
          throwError $
            Error.VariableNotFound pos (Fix $ Var pos x n) ctx
        Just t' -> return t'

    alg (Lambda pos x (te, t) (_, b)) = do
      void $ getUniverse pos te =<< whnf =<< t
      b' <- inContext x te b
      return (Fix (Pi pos x te b'))

    alg (Pi pos x (ke, k) (te, t)) = do
      u1 <- getUniverse pos ke =<< whnf =<< k
      u2 <- getUniverse pos te =<< inContext x ke (whnf =<< t)
      return (Fix (Universe pos (max u1 u2)))

    alg (App pos (fe, f) (ae, a)) = do
      (x, k', t') <- getFunction pos fe =<< whnf =<< f
      a' <- a
      checkEqualTypes pos "Function argument type mismatch."
        (Fix (App pos fe ae)) k' a'
      normalize $ subst x 0 ae t'

    alg (Universe pos u) =
      return (Fix (Universe pos (succ u)))

getUniverse :: forall m. (MonadError Err m, MonadReader Ctx m) => Position -> Expr -> Type -> m Integer
getUniverse pos term = \case
  (Fix (Universe _ u)) -> return u
  other -> throwError $ Error.TypeExpected pos term other

getFunction :: forall m. (MonadError Err m, MonadReader Ctx m) => Position -> Expr -> Type -> m (Variable, Type, Type)
getFunction pos term = \case
  (Fix (Pi _ x k t)) -> return (x, k, t)
  other -> throwError $ Error.FunctionExpected pos term other

----------------------------------------------------------------------
-- Utils

inContext :: forall m a. (MonadReader Ctx m) => Variable -> Type -> m a -> m a
inContext x t = local (Ctx.extend x t)

runEval :: (MonadError String m) => ExceptT Err (Reader Ctx) a -> Ctx -> m a
runEval act ctx =
  case runReader (runExceptT act) ctx of
    Left err -> throwError . unpack . displayT . renderPretty 1.0 120 . pretty . fmap (ppExpr 0 . sugar) $ err
    Right r  -> return r
