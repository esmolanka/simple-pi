{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}

module SPI.Value (eval, reify, equal) where

import Control.Arrow (second)

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Data.Functor.Foldable (Fix (..), cata)
import qualified Data.Map as M
import Data.Map (Map)

import SPI.Expr (Variable (..), Expr)
import qualified SPI.Expr as Expr
import SPI.Error
import SPI.Env

import Language.SimplePi.Types (dummyPos)

type EvalM = ExceptT String (StateT Int (Reader (Context, Map Variable Value)))

runEval  :: (MonadError String m, MonadReader Env m, MonadState Int m) => EvalM a -> m a
runEval m = do
  ctx <- asks gamma
  counter <- get
  let (r, ctx') = runReader (runStateT (runExceptT m) counter) (ctx, M.empty)
  put ctx'
  either throwError return r

data Value
  = Neutral Neutral
  | Universe Integer
  | Pi Variable Value (Value -> EvalM Value)
  | Lambda Variable (Maybe Value) (Value -> EvalM Value)

instance Show Value where
  show (Neutral n)    = "(Neutral " ++ show n ++ ")"
  show (Universe n)   = "(Universe " ++ show n ++ ")"
  show (Pi x _ _)     = "(Pi " ++ show x ++ " ...)"
  show (Lambda x _ _) = "(Lambda " ++ show x ++ " ...)"

data Neutral
  = Var Variable
  | App Neutral Value
  deriving (Show)

equal :: (MonadError String m, MonadReader Env m, MonadState Int m) => Value -> Value -> m Bool
equal a b = runEval (equal' a b)

equal' :: Value -> Value -> EvalM Bool
equal' va vb =
  case (va, vb) of
    (Neutral a, Neutral b)         -> eqNeutral a b
    (Universe n, Universe m)       -> return (n == m)
    (Pi x t b, Pi _ t' b')         -> eqAbs x (Just t) b (Just t') b'
    (Lambda x t b, Lambda _ t' b') -> eqAbs x t b t' b'
    _ -> return False
  where
    eqNeutral n m =
      case (n, m) of
        (Var x, Var y) -> return (x == y)
        (App n' v, App m' w) -> (&&) <$> eqNeutral n' m' <*> equal' v w
        _ -> return False

    eqAbs x mt b mt' b' = do
      ts <- case (mt, mt') of
              (Nothing, Nothing) -> return True
              (Just t, Just t')  -> equal' t t'
              (Just _, Nothing)  -> return False
              (Nothing, Just _)  -> return False
      x' <- Neutral . Var <$> Expr.refresh x
      bx1 <- b x'
      bx2 <- b' x'
      bs <- equal' bx1 bx2
      return (ts && bs)

eval :: (MonadError String m, MonadReader Env m, MonadState Int m) => Expr -> m Value
eval = runEval . eval'

eval' :: Expr -> EvalM Value
eval' = cata $ \case
  Expr.Var pos x -> do
    mv <- asks (M.lookup x . snd)
    case mv of
      Just v -> return v
      Nothing -> do
        me <- asks (lookupValue x . fst)
        case me of
          Nothing -> unknownIdentifierError pos x
          Just Nothing -> return $ Neutral (Var x)
          Just (Just e) -> eval' e

  Expr.Universe _ n -> return $ Universe n

  Expr.Pi _ x t b -> do
    t' <- t
    env' <- asks snd
    let body v = local (second (M.insert x v . const env')) b
    return $ Pi x t' body

  Expr.Lambda _ x mt b -> do
    t' <- sequence mt
    env' <- asks snd
    let body v = local (second (M.insert x v . const env')) b
    return $ Lambda x t' body

  Expr.App pos f a -> do
    f' <- f
    case f' of
      Lambda _ _ f'' -> f'' =<< a
      Neutral n -> Neutral . App n <$> a
      _ -> throwError $ displayPos pos ++ ": application to non-function"

reify :: (MonadError String m, MonadReader Env m, MonadState Int m) => Value -> m Expr
reify = runEval . reify'

reify' :: Value -> EvalM Expr
reify' = \case
  Neutral n  -> reifyNeutral n
  Universe n -> return $ Fix $ Expr.Universe dummyPos n
  Pi x t b -> do
    x' <- Expr.refresh x
    t' <- reify' t
    b' <- reify' =<< b (Neutral (Var x'))
    return $ Fix $ Expr.Pi dummyPos x' t' b'
  Lambda x t b -> do
    x' <- Expr.refresh x
    mt' <- traverse reify' t
    b' <- reify' =<< b (Neutral (Var x'))
    return $ Fix $ Expr.Lambda dummyPos x' mt' b'
  where
    reifyNeutral :: Neutral -> EvalM Expr
    reifyNeutral = \case
      Var x -> return $ Fix $ Expr.Var dummyPos x
      App n v -> do
        n' <- reifyNeutral n
        v' <- reify' v
        return $ Fix $ Expr.App dummyPos n' v'
