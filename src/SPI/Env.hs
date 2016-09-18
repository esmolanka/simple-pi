{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}

module SPI.Env where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Data.Map (Map)

import qualified Data.Map as M

import SPI.Expr
import Language.SimplePi.Types (dummyPos)

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
  [ (VarStr "Type",  (Fix $ Universe dummyPos 1, Just $ Fix $ Universe dummyPos 0))
  , (VarStr "Type₁", (Fix $ Universe dummyPos 2, Just $ Fix $ Universe dummyPos 1))
  , (VarStr "Type₂", (Fix $ Universe dummyPos 3, Just $ Fix $ Universe dummyPos 2))
  , (VarStr "Type₃", (Fix $ Universe dummyPos 4, Just $ Fix $ Universe dummyPos 3))
  ]

data Env = Env
  { gamma      :: Context
  , annotation :: TypeExpectation
  }

data TypeExpectation
  = Any
  | LamArg Expr TypeExpectation
  | Exactly Expr

infix 2 ?:

(?:) :: forall a m. (MonadError String m, MonadReader Env m, MonadState Int m) => m a -> TypeExpectation -> m a
(?:) tc annot = local (\env -> env { annotation = annot }) tc

getAnnot :: forall m. (MonadError String m, MonadReader Env m, MonadState Int m) => m TypeExpectation
getAnnot = asks annotation

inContext :: forall a m. (MonadError String m, MonadReader Env m, MonadState Int m) => Variable -> Expr -> m a -> m a
inContext var typ = local (\env -> env { gamma = extendCtx var typ Nothing (gamma env)})

freshEnv :: Env
freshEnv = Env freshCtx Any
