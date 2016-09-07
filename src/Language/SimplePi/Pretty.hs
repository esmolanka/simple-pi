{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.SimplePi.Pretty where

import Data.Text.Lazy (fromStrict, pack)
import Data.Char
import Text.PrettyPrint.Leijen.Text

import Language.SimplePi.Types

ppIdent :: Ident -> Doc
ppIdent (Ident n) = text (fromStrict n)

ppBnd :: Binding Maybe Expr -> Doc
ppBnd (Binding idn (Just e)) =
  parens (ppIdent idn <+> colon <+> ppExpr 0 e)
ppBnd (Binding idn Nothing) =
  ppIdent idn

ppBnd' :: Binding Identity Expr -> Doc
ppBnd' (Binding idn (Identity e)) =
  parens (ppIdent idn <+> colon <+> ppExpr 0 e)

ppExpr :: Int -> Expr -> Doc
ppExpr n (Fix e) =
  ppExprF n e

ppExprF :: Int -> ExprF Expr -> Doc
ppExprF p expr =
  case expr of
    App _ a b rest     -> pParen (p >= 8) $
      fillSep $ map (ppExpr 8) (a : b : rest)

    Pi _ bnd e         -> pParen (p >= 5) $
      ppBnd' bnd <+> text "->" <+> nest 2 (ppExpr 3 e)

    Arrow _ a b rest   ->
      let (h : t) = reverse (a : b : rest)
      in foldr ppArrow (ppExpr 3 h) t
      where
        ppArrow :: Expr -> Doc -> Doc
        ppArrow e tail = pParen (p >= 4) $ ppExpr 5 e <+> text "->" <+> tail

    Lambda   _ bnds e  -> pParen (p >= 1) $
      text "\\" <> fillSep (map ppBnd bnds) <+> text "=>" <+> nest 2 (ppExpr 1 e)

    Annot _ e t        -> pParen (p >= 0) $
      parens (ppExpr 0 e) <+> colon <> colon <+> ppExpr 0 t

    Var _ idn ->
      ppIdent idn

    Universe _ n ->
      if n > 0 then text "Type" <> (text $ pack $ mkSubscript $ show n)
               else text "Type"
  where
    pParen :: Bool -> Doc -> Doc
    pParen True  = parens
    pParen False = id

mkSubscript :: String -> String
mkSubscript =
  map $ \c ->
    let code = ord c in
    if code >= 48 && code <= 57
    then chr (code - 48 + 8320)
    else c
