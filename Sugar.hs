{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveFunctor     #-}

module Sugar where

import Expr

import Data.Functor.Foldable (Fix (..), cata)

import GHC.Generics (Generic)
import Language.Sexp (Position)

type Sugared = Fix SugaredF

data Binding e = Binding Variable e
    deriving (Show, Eq, Ord, Functor, Generic)

data SugaredF e
  = SLambda   Position [Binding e] e
  | SPi       Position (Binding e) e
  | SArrow    Position e e [e]
  | SApp      Position e e [e]
  | SVar      Position Variable
  | SUniverse Position Int
    deriving (Show, Eq, Ord, Functor, Generic)

desugar :: Sugared -> Expr
desugar = cata alg
  where
    alg (SLambda pos bnds body) =
      foldr (\(Binding var e) rest -> Fix $ Lambda pos var e rest) body bnds
    alg (SPi pos (Binding var ty) body) =
      Fix $ Pi pos var ty body
    alg (SArrow pos a b rest) =
      let es = a : b : rest
      in foldr (\e rest -> Fix $ Pi pos Dummy e rest) (last es) (init es)
    alg (SApp pos a b rest) =
      foldl (\acc e -> Fix $ App pos acc e) a (b:rest)
    alg (SVar pos var) =
      Fix $ Var pos var
    alg (SUniverse pos u) =
      Fix $ Universe pos u

-- sugar :: Expr -> Sugared
-- sugar = futu coalg
--   where
--     coalg :: Expr -> SugaredF (Free SugaredF Expr)
--     coalg = undefined
