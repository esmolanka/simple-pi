{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE OverloadedStrings #-}

module SimpTT.Sugar where

import Data.Text (pack)
import Data.Functor.Identity
import Data.Functor.Foldable (Fix (..), cata, para)
import Data.Maybe
import Data.Monoid ((<>))

import SimpTT.Expr
import qualified Language.SimplePi.Types as AST

desugar :: AST.Expr -> Expr
desugar = cata alg
  where
    alg (AST.Lambda pos bnds body) =
      foldr (\(AST.Binding var e) rest -> Fix $ Lambda pos (desugarIdent var) (fromMaybe (hole pos) e) rest) body bnds
    alg (AST.Pi pos (AST.Binding var (Identity ty)) body) =
      Fix $ Pi pos (desugarIdent var) ty body
    alg (AST.Arrow pos a b rest) =
      let es = a : b : rest
      in foldr (\e rest -> Fix $ Pi pos (Variable "_") e rest) (last es) (init es)
    alg (AST.App pos a b rest) =
      foldl (\acc e -> Fix $ App pos acc e) a (b:rest)
    alg (AST.Var pos var) =
      Fix $ Var pos (desugarIdent var) 0
    alg (AST.Universe pos u) =
      Fix $ Universe pos u

    hole :: AST.Position -> Expr
    hole pos = Fix $ Var pos (Variable "?hole") 0

desugarIdent :: AST.Ident -> Variable
desugarIdent (AST.Ident t) = Variable t

sugar :: Expr -> AST.Expr
sugar = para alg
  where
    alg :: ExprF (Expr, AST.Expr) -> AST.Expr
    alg (Pi pos x t b)
      | free x 0 (fst b)   = Fix $ AST.Pi pos (AST.Binding (sugarIdent x) (Identity (snd t))) (snd b)
      | otherwise          = sugarArrow pos (snd t) (snd b)
    alg (Lambda pos x t b) = sugarLambda pos (sugarIdent x) (Just (snd t)) (snd b)
    alg (App pos f a)      = sugarApply pos (snd f) (snd a)
    alg (Var pos v n)      = Fix $ AST.Var pos (sugarVar v n)
    alg (Universe pos n)   = Fix $ AST.Universe pos n

    sugarLambda pos x t (Fix (AST.Lambda _ bnds body)) = Fix $ AST.Lambda pos (AST.Binding x t : bnds) body
    sugarLambda pos x t body = Fix $ AST.Lambda pos [AST.Binding x t] body

    sugarArrow pos t (Fix (AST.Arrow _ a b cs)) = Fix $ AST.Arrow pos t a (b : cs)
    sugarArrow pos t b = Fix (AST.Arrow pos t b [])

    sugarApply pos (Fix (AST.App _ f a bs)) c = Fix $ AST.App pos f a (bs ++ [c])
    sugarApply pos f a = Fix $ AST.App pos f a []

sugarIdent :: Variable -> AST.Ident
sugarIdent (Variable x) = AST.Ident x

sugarVar :: Variable -> Integer -> AST.Ident
sugarVar (Variable x) n
  | n > 0 = AST.Ident (x <> "/" <> pack (show n))
  | otherwise = AST.Ident x
