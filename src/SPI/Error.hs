{-# LANGUAGE FlexibleContexts    #-}

module SPI.Error where

import Control.Monad.Except

import SPI.Expr
import SPI.Pretty

import Language.SimplePi.Types (Position, dummyPos)

unknownIdentifierError :: (MonadError String m) => Position -> Variable -> m a
unknownIdentifierError pos var =
  throwError $ displayPos pos ++ ": unknown identifier: " ++ displayExpr (Fix $ Var dummyPos var)

typesDontMatchError :: (MonadError String m) => Position -> Expr -> Expr -> m a
typesDontMatchError pos t1 t2 =
  throwError $ displayPos pos ++ ": types do not match:\n" ++ displayExpr t1 ++ "\n" ++ displayExpr t2

getUniverse :: (MonadError String m) => Expr -> m Integer
getUniverse (Fix (Universe _pos x)) = return x
getUniverse other = throwError $ displayPos (getPos other) ++ ": type expected:\n" ++ displayExpr other

getPi  :: (MonadError String m) => Position -> String -> Expr -> m (Variable, Expr, Expr)
getPi _ _ (Fix (Pi _pos x t1 t2)) = return (x, t1, t2)
getPi pos msg other = throwError $ displayPos pos ++ ": " ++ msg ++ "; Function expected:\n" ++ displayExpr other
