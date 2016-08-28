{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}

module SPI.Sugar where

import Data.Foldable
import Data.Functor.Identity
import Data.Functor.Foldable (cata)
import Data.Monoid (Any (..))
import GHC.Generics (Generic)

import SPI.Expr

type Sugared = Fix SugaredF

data Binding f e = Binding Variable (f e)
    deriving (Show, Eq, Ord, Foldable, Functor, Generic)

data SugaredF e
  = SLambda   Position [Binding Maybe e] e
  | SPi       Position (Binding Identity e) e
  | SArrow    Position e e [e]
  | SApp      Position e e [e]
  | SAnnot    Position e e
  | SVar      Position Variable
  | SUniverse Position Int
    deriving (Show, Eq, Ord, Functor, Foldable, Generic)

desugar :: Sugared -> Expr
desugar = cata alg
  where
    alg (SLambda pos bnds body) =
      foldr (\(Binding var e) rest -> Fix $ Lambda pos var e rest) body bnds
    alg (SPi pos (Binding var (Identity ty)) body) =
      Fix $ Pi pos var ty body
    alg (SArrow pos a b rest) =
      let es = a : b : rest
      in foldr (\e rest -> Fix $ Pi pos Dummy e rest) (last es) (init es)
    alg (SApp pos a b rest) =
      foldl (\acc e -> Fix $ App pos acc e) a (b:rest)
    alg (SAnnot pos e t) =
      Fix $ Annot pos e t
    alg (SVar pos var) =
      Fix $ Var pos var
    alg (SUniverse pos u) =
      Fix $ Universe pos u

sugar :: Expr -> Sugared
sugar = cata alg
  where
    alg :: ExprF Sugared -> Sugared

    alg (Lambda pos x t b) = sugarLambda pos x t b

    alg (Pi pos x t b)
      | uses x b           = Fix $ SPi pos (Binding x (Identity t)) b
      | otherwise          = sugarArrow pos t b

    alg (App pos f a)      = sugarApply pos f a

    alg (Annot pos e t)    = Fix $ SAnnot pos e t

    alg (Var pos v)        = Fix $ SVar pos v
    alg (Universe pos n)   = Fix $ SUniverse pos n

    sugarLambda pos x t (Fix (SLambda _ bnds body)) = Fix $ SLambda pos (Binding x t : bnds) body
    sugarLambda pos x t body = Fix $ SLambda pos [Binding x t] body

    sugarArrow pos t (Fix (SArrow _ a b cs)) = Fix $ SArrow pos t a (b : cs)
    sugarArrow pos t b = Fix (SArrow pos t b [])

    sugarApply pos (Fix (SApp _ f a bs)) c = Fix $ SApp pos f a (bs ++ [c])
    sugarApply pos f a = Fix $ SApp pos f a []

uses :: Variable -> Sugared -> Bool
uses var = getAny . cata alg
  where
    alg (SVar _ var') = Any (var == var')
    alg e = fold e
