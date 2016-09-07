{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE OverloadedStrings #-}

module SPI.Sugar
  ( module SPI.Sugar
  , Fix (..)
  ) where

import Data.Text (pack)
import Data.Foldable
import Data.Functor.Identity
import Data.Functor.Foldable (cata)
import Data.Monoid (Any (..), (<>))

import SPI.Expr
import qualified Language.SimplePi.Types as AST

desugar :: AST.Expr -> Expr
desugar = cata alg
  where
    alg (AST.Lambda pos bnds body) =
      foldr (\(AST.Binding var e) rest -> Fix $ Lambda pos (desugarIdent var) e rest) body bnds
    alg (AST.Pi pos (AST.Binding var (Identity ty)) body) =
      Fix $ Pi pos (desugarIdent var) ty body
    alg (AST.Arrow pos a b rest) =
      let es = a : b : rest
      in foldr (\e rest -> Fix $ Pi pos Dummy e rest) (last es) (init es)
    alg (AST.App pos a b rest) =
      foldl (\acc e -> Fix $ App pos acc e) a (b:rest)
    alg (AST.Var pos var) =
      Fix $ Var pos (desugarIdent var)
    alg (AST.Universe pos u) =
      Fix $ Universe pos u

desugarIdent :: AST.Ident -> Variable
desugarIdent (AST.Ident t) =
  case t of
    "_" -> Dummy
    _   -> VarStr t

sugar :: Expr -> AST.Expr
sugar = cata alg
  where
    alg :: ExprF (AST.Expr) -> AST.Expr
    alg (Pi pos x t b)
      | uses (sugarIdent x) b = Fix $ AST.Pi pos (AST.Binding (sugarIdent x) (Identity t)) b
      | otherwise             = sugarArrow pos t b
    alg (Lambda pos x t b)    = sugarLambda pos (sugarIdent x) t b
    alg (App pos f a)         = sugarApply pos f a
    alg (Var pos v)           = Fix $ AST.Var pos (sugarIdent v)
    alg (Universe pos n)      = Fix $ AST.Universe pos n

    sugarLambda pos x t (Fix (AST.Lambda _ bnds body)) = Fix $ AST.Lambda pos (AST.Binding x t : bnds) body
    sugarLambda pos x t body = Fix $ AST.Lambda pos [AST.Binding x t] body

    sugarArrow pos t (Fix (AST.Arrow _ a b cs)) = Fix $ AST.Arrow pos t a (b : cs)
    sugarArrow pos t b = Fix (AST.Arrow pos t b [])

    sugarApply pos (Fix (AST.App _ f a bs)) c = Fix $ AST.App pos f a (bs ++ [c])
    sugarApply pos f a = Fix $ AST.App pos f a []

sugarIdent :: Variable -> AST.Ident
sugarIdent (VarStr x) = AST.Ident x
sugarIdent (GenSym x n) = AST.Ident $ x <> pack "_" <> pack (show n)
sugarIdent Dummy = AST.Ident (pack "_")

uses :: AST.Ident -> AST.Expr -> Bool
uses var = getAny . cata alg
  where
    alg (AST.Var _ var') = Any (var == var')
    alg e = fold e
