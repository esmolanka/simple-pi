{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module SPI.Grammar where

import Control.Category ((>>>))
import Data.Coerce
import GHC.Generics (Generic)

import Language.SexpGrammar
import Language.SexpGrammar.Generic

import SPI.Expr
import SPI.Sugar

----------------------------------------------------------------------
-- Statement grammar

data Statement
  = Definition Variable Sugared {- <var> = <expr> -}
  | Parameter  Variable Sugared {- <var> : <expr> -}
  | Check      Sugared
  | Eval       Sugared
  | AssertType Sugared Sugared
    deriving (Generic)

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
-- sugared grammars

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
      position >>> swap >>>
      list (
        el (sym "lambda")   >>>
        el (list (rest (bindingGrammar sugaredGrammar))) >>>
        el sugaredGrammar) >>>
      slambda)
  $ With (\spi ->
      position >>> swap >>>
      list (
        el (sym "forall")  >>>
        el (bindingGrammar sugaredGrammar) >>>
        el (sym "->")      >>>
        el sugaredGrammar) >>>
      spi)
  $ With (\sarrow ->
      position >>> swap >>>
      list (
        el (sym "->") >>>
        el sugaredGrammar >>>
        el sugaredGrammar >>>
        rest sugaredGrammar) >>>
      sarrow)
  $ With (\sapp ->
      position >>> swap >>>
      list (
        el sugaredGrammar >>>
        el sugaredGrammar >>>
        rest sugaredGrammar) >>>
      sapp)
  $ With (\svar ->
        position >>> swap >>>
        variableGrammar >>>
        svar)
  $ With (\suniv ->
      position >>> swap >>>
      starGrammar >>>
      suniv)
  $ End

----------------------------------------------------------------------
-- desugared grammars

expressionGrammar :: SexpG Expr
expressionGrammar = fixG $ match
  $ With (\var  ->
      position        >>>
      swap            >>>
      variableGrammar >>> var)
  $ With (\univ ->
      position        >>>
      swap            >>>
      starGrammar     >>> univ)

  $ With (\pi ->
      position >>>
      swap     >>>
      list (
        el (sym "forall")     >>>
        el variableGrammar    >>>
        el (kw (Kw ":"))      >>>
        el expressionGrammar  >>>
        el (sym "->")         >>>
        el expressionGrammar) >>> pi)
  $ With (\lam  ->
      position >>>
      swap     >>>
      list (
        el (sym "lambda")     >>>
        el variableGrammar    >>>
        el (kw (Kw ":"))      >>>
        el expressionGrammar  >>>
        el (sym ".")         >>>
        el expressionGrammar) >>> lam)
  $ With (\app  ->
      position >>>
      swap     >>>
      list (
        el expressionGrammar  >>>
        el expressionGrammar) >>> app)
  $ End

variableGrammar :: SexpG Variable
variableGrammar = match
  $ With (\str -> symbol' >>> str)
  $ With (\strn -> vect (el symbol' >>> el int) >>> strn)
  $ With (\dummy -> sym "_" >>> dummy)
  $ End

starGrammar :: SexpG Int
starGrammar = list (el (sym "type") >>> el int)

----------------------------------------------------------------------
-- Utils

-- TODO : Doesn't work properly for generation

-- consGrammar :: Grammar g (([a], a) :- t) ([a] :- t)
-- consGrammar = partialIso "list" cons uncons
--   where
--     cons = uncurry $ flip (:)
--     uncons [] = Left (unexpected "empty list")
--     uncons (x:xs) = Right (xs, x)

-- addElem
--   :: Grammar SexpGrammar (Sexp :- [a] :- t) (a :- [a] :- t)
--   -> Grammar SeqGrammar ([a] :- t) ([a] :- t)
-- addElem g = el g >>> pair >>> consGrammar

-- terminatedBy
--   :: (Eq a) =>
--      Grammar SexpGrammar (Sexp :- [a] :- t) (a :- [a] :- t)
--   -> Grammar SeqGrammar ([a] :- t) t'
--   -> Grammar SeqGrammar t t'
-- terminatedBy f g =
--   push [] >>> go f g
--   where
--     go f g = (addElem f >>> go f g)
--           <> (iso reverse reverse >>> g)

fixG :: Grammar SexpGrammar (Sexp :- t) (f (Fix f) :- t)
     -> Grammar SexpGrammar (Sexp :- t) (Fix f :- t)
fixG g = g >>> iso coerce coerce
