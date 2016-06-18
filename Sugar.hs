{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveFunctor     #-}

module Sugar where

import Expr
import GHC.Generics (Generic)

newtype Fix f = Fix { unFix :: f (Fix f) }

cata :: (Functor f) => (f a -> a) -> Fix f -> a
cata alg = go
  where
    go = alg . fmap go . unFix

type Sugared = Fix SugaredF

data Binding e = Binding Variable e
    deriving (Show, Eq, Ord, Functor, Generic)

data SugaredF e
  = SLambda   [Binding e] e
  | SPi       (Binding e) e
  | SArrow    e e [e]
  | SApp      e e [e]
  | SVar      Variable
  | SUniverse Int
    deriving (Show, Eq, Ord, Functor, Generic)

desugar :: Sugared -> Expr
desugar = cata alg
  where
    alg (SLambda bnds body) =
      foldr (\(Binding var e) rest -> Lambda (Abstraction var e rest)) body bnds
    alg (SPi (Binding var ty) body) =
      Pi (Abstraction var ty body)
    alg (SArrow a b rest) =
      let es = a : b : rest
      in foldr (\e rest -> Pi (Abstraction Dummy e rest)) (last es) (init es)
    alg (SApp a b rest) =
      foldl App a (b:rest)
    alg (SVar var) =
      Var var
    alg (SUniverse u) =
      Universe u


----------------------------------------------------------------------

data Statement
  = Definition Variable Sugared {- <var> = <expr> -}
  | Parameter  Variable Sugared {- <var> : <expr> -}
  | Check      Sugared
  | Type       Sugared
    deriving (Generic)
