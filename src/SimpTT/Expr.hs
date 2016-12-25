{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE LambdaCase          #-}

module SimpTT.Expr
  ( Variable (..)
  , ExprF (..)
  , Expr
  , getPosition
  , free
  , shift
  , subst
  , areEqual
  -- Constructors
  , var
  , lam
  , forall
  , app
  , universe
  ) where

import Control.Arrow (first, second)
import Control.Monad.Reader

import Data.Foldable
import Data.Functor.Foldable (Fix (..), cata)
import Data.Monoid (Any (..), (<>))
import Data.Text (Text, pack)
import Data.String

import GHC.Generics (Generic)
import Language.SimplePi.Types (Position, dummyPos)

newtype Variable = Variable Text
  deriving (Show, Eq, Ord, Generic)

instance IsString Variable where
  fromString = Variable . pack

data ExprF e
  = Var      Position !Variable !Integer    -- x
  | Lambda   Position !Variable e e         -- ƛ x:A. e
  | Pi       Position !Variable e e         -- (x:A) → B
  | App      Position e e                   -- f a
  | Universe Position !Integer              -- Type₀, Type₁, ...
    deriving (Functor, Generic, Foldable, Traversable)

var :: Variable -> Integer -> Expr
var x n = Fix $ Var dummyPos x n

lam :: Variable -> Expr -> Expr -> Expr
lam x t b = Fix $ Lambda dummyPos x t b

forall :: Variable -> Expr -> Expr -> Expr
forall x k t = Fix $ Pi dummyPos x k t

app :: Expr -> Expr -> Expr
app f a = Fix $ App dummyPos f a

universe :: Integer -> Expr
universe n = Fix $ Universe dummyPos n

getPosition :: Expr -> Position
getPosition (Fix (Var pos _ _)) = pos
getPosition (Fix (Universe pos _)) = pos
getPosition (Fix (Pi pos _ _ _)) = pos
getPosition (Fix (Lambda pos _ _ _)) = pos
getPosition (Fix (App pos _ _)) = pos

type Expr = Fix ExprF

free :: Variable -> Integer -> Expr -> Bool
free x n0 expr = getAny $ runReader (cata alg expr) n0
  where
    alg :: ExprF (Reader Integer Any) -> Reader Integer Any
    alg (Var _ x' n') = do
      n <- ask
      return $ Any (x == x' && n == n')
    alg (Lambda _ x' t b) = do
      t' <- t
      b' <- if x == x' then local succ b else b
      return $ t' <> b'
    alg (Pi _ x' k t) = do
      k' <- k
      t' <- if x == x' then local succ t else t
      return $ k' <> t'
    alg e = fold <$> sequence e

shift :: Integer -> Variable -> Expr -> Expr
shift d x e = runReader (cata alg e) 0
  where
    alg :: ExprF (Reader Integer Expr) -> Reader Integer Expr
    alg = \case
      Var pos x' n -> do
        c <- ask
        return $ Fix $ Var pos x' $
          if x == x' && n >= c then n + d else n
      Lambda pos x' a b -> do
        a' <- a
        b'  <- if x == x' then local succ b else b
        return $ Fix $ Lambda pos x' a' b'
      Pi pos x' a b -> do
        a' <- a
        b'  <- if x == x' then local succ b else b
        return $ Fix $ Pi pos x' a' b'
      other -> Fix <$> sequence other

subst :: Variable -> Integer -> Expr -> Expr -> Expr
subst x n0 sub0 expr = runReader (cata alg expr) (n0, sub0)
  where
    succIndex :: Reader (Integer, Expr) a -> Reader (Integer, Expr) a
    succIndex = local (first succ)

    shifted :: Integer -> Variable -> Reader (Integer, Expr) a -> Reader (Integer, Expr) a
    shifted d x = local (second (shift d x))

    alg :: ExprF (Reader (Integer, Expr) Expr) -> Reader (Integer, Expr) Expr
    alg = \case
      Var pos x' n' -> do
        (n, sub) <- ask
        if x' == x && n' == n
          then return sub
          else return (Fix (Var pos x' n'))
      Lambda pos x' a b -> do
        a' <- a
        b' <- shifted 1 x' $
          if x == x'
          then succIndex b
          else b
        return (Fix (Lambda pos x' a' b'))
      Pi pos x' a b -> do
        a' <- a
        b' <- shifted 1 x' $
          if x == x'
          then succIndex b
          else b
        return (Fix (Pi pos x' a' b'))
      other -> Fix <$> sequence other

areEqual :: Expr -> Expr -> Bool
areEqual a b = runReader (go a b) []
  where
    go :: Expr -> Expr -> Reader [(Variable, Variable)] Bool
    go (Fix a) (Fix b) =
      case (a, b) of
        (Var _ x n, Var _ x' n') ->
          match (x, n) (x', n')
        (Lambda _ x t b, Lambda _ x' t' b') ->
          (&&) <$> go t t' <*> push x x' (go b b')
        (Pi _ x k t, Pi _ x' k' t') ->
          (&&) <$> go k k' <*> push x x' (go t t')
        (App _ f a, App _ f' a') ->
          (&&) <$> go f f' <*> go a a'
        (Universe _ u, Universe _ u') ->
          return (u == u')
        (_, _) ->
          return False

    push :: Variable -> Variable -> Reader [(Variable, Variable)] a -> Reader [(Variable, Variable)] a
    push x x' = local ((x, x') :)

    match :: (Variable, Integer) -> (Variable, Integer) -> Reader [(Variable, Variable)] Bool
    match (x, n) (y, m) = go 0 0 <$> ask
      where
        go :: Integer -> Integer -> [(Variable, Variable)] -> Bool
        go !n' !m' = \case
          ((x', y') : rest) ->
            x == x' && y == y' && n == n' && m == m' ||
            go (if x == x' then succ n' else n')
               (if y == y' then succ m' else m')
               rest
          [] ->
            x == y && n' == m'
