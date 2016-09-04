{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module SPI.Grammar (displayExpr) where

import Control.Category ((>>>))

import Data.Coerce
import Data.Functor.Identity
import qualified Data.Text.Lazy as T

import Language.SexpGrammar
import Language.SexpGrammar.Generic

import SPI.Expr
import SPI.Sugar

import qualified Language.SimplePi.Types as AST

displayExpr :: Expr -> String
displayExpr =
  either ("printing error: " ++) (T.unpack) .
    encodePrettyWith sugaredGrammar . sugar

----------------------------------------------------------------------
-- sugared grammars

bindingGrammar
  :: Grammar SexpGrammar (Sexp :- AST.Ident :- t) (e :- AST.Ident :- t)
  -> Grammar SexpGrammar (Sexp :- t) (AST.Binding Identity e :- t)
bindingGrammar g = with $ \binding ->
  list (
    el variableGrammar  >>>
    el (kw (Kw ":"))    >>>
    el (g >>> iso Identity runIdentity)) >>>
  binding

bindingGrammar'
  :: Grammar SexpGrammar (Sexp :- AST.Ident :- t) (e :- AST.Ident :- t)
  -> Grammar SexpGrammar (Sexp :- t) (AST.Binding Maybe e :- t)
bindingGrammar' g = with $ \binding ->
  coproduct
    [ variableGrammar  >>>
      pushForget Nothing
    , list (
        el variableGrammar  >>>
        el (kw (Kw ":"))    >>>
        el (g >>> just)
        )
    ] >>> binding
  where
    just = partialIso "Just" Just unJust
    unJust Nothing = Left $ unexpected "Nothing"
    unJust (Just a) = Right a

sugaredGrammar :: SexpG AST.Expr
sugaredGrammar = fixG $ match
  $ With (\slambda ->
      position'             >>> swap >>>
      list (
        el (sym "lambda")   >>>
        el (list (rest (bindingGrammar' sugaredGrammar))) >>>
        el sugaredGrammar) >>>
      slambda)
  $ With (\spi ->
      position'              >>> swap >>>
      list (
        el (sym "forall")    >>>
        el (bindingGrammar sugaredGrammar) >>>
        el (sym "->")        >>>
        el sugaredGrammar)   >>>
      spi)
  $ With (\sarrow ->
      position'              >>> swap >>>
      list (
        el (sym "->")        >>>
        el sugaredGrammar    >>>
        el sugaredGrammar    >>>
        rest sugaredGrammar) >>>
      sarrow)
  $ With (\sapp ->
      position'              >>> swap >>>
      list (
        el sugaredGrammar    >>>
        el sugaredGrammar    >>>
        rest sugaredGrammar) >>>
      sapp)
  $ With (\sannot ->
      position'              >>>
      swap                   >>>
      vect (
        el sugaredGrammar    >>>
        el sugaredGrammar)   >>>
      sannot)
  $ With (\svar ->
      position'              >>> swap >>>
      variableGrammar        >>>
      svar)
  $ With (\suniv ->
      position'              >>> swap >>>
      starGrammar            >>>
      suniv)
  $ End

variableGrammar :: SexpG AST.Ident
variableGrammar = with (\str -> symbol >>> str)

position' :: Grammar SexpGrammar (Sexp :- t) (b :- (Sexp :- t))
position' = position >>> iso undefined undefined

starGrammar :: SexpG Integer
starGrammar = list (el (sym "type") >>> el integer)

----------------------------------------------------------------------
-- Utils

fixG :: Grammar SexpGrammar (Sexp :- t) (f (Fix f) :- t)
     -> Grammar SexpGrammar (Sexp :- t) (Fix f :- t)
fixG g = g >>> iso coerce coerce
