{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SPI.Expr
  ( Expr
  , ExprF (..)
  , Variable (..)
  , Fix (..)
  , unFix
  , getPos
  , subst
  , refresh
  ) where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Data.Text (Text)
import Data.Coerce
import Data.Functor.Foldable (Fix (..), cata)
import Data.Map (Map)
import qualified Data.Map as M

import GHC.Generics (Generic)

import Language.SimplePi.Types (Position)

data Variable
  = VarStr Text
  | GenSym Text Int
  | Dummy
    deriving (Show, Eq, Ord, Generic)

type Expr = Fix ExprF

data ExprF e
  = Var      Position Variable              -- x
  | Universe Position Integer               -- Type₀, Type₁, ...
  | Pi       Position Variable e e          -- (x:A) → B
  | Lambda   Position Variable (Maybe e) e  -- ƛ x:A. e
  | App      Position e e                   -- f a
    deriving (Show, Eq, Ord, Functor, Generic, Foldable, Traversable)

getPos :: Expr -> Position
getPos (Fix (Var pos _)) = pos
getPos (Fix (Universe pos _)) = pos
getPos (Fix (Pi pos _ _ _)) = pos
getPos (Fix (Lambda pos _ _ _)) = pos
getPos (Fix (App pos _ _)) = pos

----------------------------------------------------------------------
-- Variables

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
      ty' <- sequence ty
      body' <- local (M.insert x (Fix $ Var pos x')) body
      return $ Fix $ Lambda pos x' ty' body'
    alg (App pos e1 e2) = do
      e1' <- e1
      e2' <- e2
      return $ Fix $ App pos e1' e2'

refresh :: forall m. (MonadState Int m) => Variable -> m Variable
refresh (VarStr x)   = modify succ >> GenSym x   <$> get
refresh (GenSym x _) = modify succ >> GenSym x   <$> get
refresh Dummy        = modify succ >> GenSym "_" <$> get

----------------------------------------------------------------------
-- Utils

unFix :: Fix f -> f (Fix f)
unFix = coerce
