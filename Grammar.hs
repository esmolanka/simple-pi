{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Grammar where

import Control.Category ((>>>))
import Control.Monad.State.Strict

import Data.Text (Text)
import Data.Semigroup
import Data.Coerce

import Language.SexpGrammar
import Language.SexpGrammar.Generic

import Expr
import Sugar

----------------------------------------------------------------------
-- Statement grammar

statementGrammar :: SexpG Statement
statementGrammar = match
  $ With (\defn ->
      list (
        el (sym "Definition") >>>
        el variableGrammar    >>>
        el sugaredGrammar)    >>> defn)
  $ With (\param ->
      list (
        el (sym "Parameter")  >>>
        el variableGrammar    >>>
        el sugaredGrammar)    >>> param)
  $ With (\check ->
      list (
        el (sym "Check")      >>>
        el sugaredGrammar)    >>> check)
  $ With (\eval ->
      list (
        el (sym "Eval")       >>>
        el sugaredGrammar)    >>> eval)
  $ With (\assert ->
      list (
        el (sym "AssertType") >>>
        el sugaredGrammar     >>>
        el sugaredGrammar)    >>> assert)
  $ End

----------------------------------------------------------------------
-- Sugared grammars

bindingGrammar
  :: Grammar SexpGrammar (Sexp :- Variable :- t) (e :- Variable :- t)
  -> Grammar SexpGrammar (Sexp :- t) (Binding e :- t)
bindingGrammar g = with $ \binding ->
  list (
    el variableGrammar >>>
    el (kw (Kw ":"))   >>>
    el g)              >>>
  binding

sugaredGrammar :: SexpG Sugared
sugaredGrammar = fixG $ match
  $ With (\slambda ->
      list (
        el (sym "lambda") >>>
        terminatedBy
          (bindingGrammar sugaredGrammar)
          (el (sym ".") >>> el sugaredGrammar)) >>>
      slambda)
  $ With (\spi ->
      list (
        el (sym "forall")  >>>
        el (bindingGrammar sugaredGrammar) >>>
        el (sym "->")      >>>
        el sugaredGrammar) >>>
      spi)
  $ With (\sarrow ->
      list (
        el (sym "->") >>>
        el sugaredGrammar >>>
        el sugaredGrammar >>>
        rest sugaredGrammar) >>>
      sarrow)
  $ With (\sapp ->
      list (
        el sugaredGrammar >>>
        el sugaredGrammar >>>
        rest sugaredGrammar) >>>
      sapp)
  $ With (\svar ->
      variableGrammar >>>
      svar)
  $ With (\suniv ->
      int >>>
      suniv)
  $ End

----------------------------------------------------------------------
-- Desugared grammars

expressionGrammar :: SexpG Expr
expressionGrammar = match
  $ With (\var  -> variableGrammar >>> var)
  $ With (\univ -> int >>> univ)
  $ With (\pi   -> abstractionGrammar "pi" >>> pi)
  $ With (\lam  -> abstractionGrammar "lambda" >>> lam)
  $ With (\app  -> list (el expressionGrammar >>> el expressionGrammar) >>> app )
  $ End

variableGrammar :: SexpG Variable
variableGrammar = match
  $ With (\str -> symbol' >>> str)
  $ With (\strn -> vect (el symbol' >>> el int) >>> strn)
  $ With (\dummy -> sym "_" >>> dummy)
  $ End

abstractionGrammar :: Text -> SexpG Abstraction
abstractionGrammar abskind = with $ \abs ->
  list (
    el (sym abskind)     >>>
    el variableGrammar   >>>
    el (kw (Kw ":"))     >>>
    el expressionGrammar >>>
    el (sym "->")        >>>
    el expressionGrammar ) >>>
  abs

----------------------------------------------------------------------
-- Utils

consGrammar :: Grammar g (([a], a) :- t) ([a] :- t)
consGrammar = partialIso "list" cons uncons
  where
    cons = uncurry $ flip (:)
    uncons [] = Left (unexpected "empty list")
    uncons (x:xs) = Right (xs, x)

addElem
  :: Grammar SexpGrammar (Sexp :- [a] :- t) (a :- [a] :- t)
  -> Grammar SeqGrammar ([a] :- t) ([a] :- t)
addElem g = el g >>> pair >>> consGrammar

terminatedBy
  :: Grammar SexpGrammar (Sexp :- [a] :- t) (a :- [a] :- t)
  -> Grammar SeqGrammar ([a] :- t) t'
  -> Grammar SeqGrammar t t'
terminatedBy f g =
  pushForget [] >>> fix (go f g)
  where
    go f g r = (addElem f >>> r)
            <> (iso reverse reverse >>> g)

fixG :: Grammar SexpGrammar (Sexp :- t) (f (Fix f) :- t)
     -> Grammar SexpGrammar (Sexp :- t) (Fix f :- t)
fixG g = g >>> iso coerce coerce
