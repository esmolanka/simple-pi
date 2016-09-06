{-# LANGUAGE FlexibleContexts    #-}

module SPI.Error where

import Control.Monad.Except
import Data.List
import Data.Text (unpack)
import qualified Data.Text.Lazy as Lazy (unpack)

import Language.SimplePi.Types (Position (..), dummyPos)
import Language.SimplePi (prettyExpr)
import SPI.Expr
import SPI.Sugar

displayPos :: Position -> String
displayPos (Position file line col) =
  intercalate ":" [ unpack file, show line, show col ]

pp :: Expr -> String
pp = Lazy.unpack . prettyExpr . sugar

unknownIdentifierError :: (MonadError String m) => Position -> Variable -> m a
unknownIdentifierError pos var =
  throwError $ displayPos pos ++ ": unknown identifier: " ++ pp (Fix $ Var dummyPos var)

typesDontMatchError :: (MonadError String m) => Position -> Expr -> Expr -> m a
typesDontMatchError pos t1 t2 =
  throwError $ displayPos pos ++ ": types do not match:\n" ++ pp t1 ++ "\n" ++ pp t2

getUniverse :: (MonadError String m) => Expr -> m Integer
getUniverse (Fix (Universe _pos x)) = return x
getUniverse other = throwError $ displayPos (getPos other) ++ ": type expected:\n" ++ pp other

getPi  :: (MonadError String m) => Position -> String -> Expr -> m (Variable, Expr, Expr)
getPi _ _ (Fix (Pi _pos x t1 t2)) = return (x, t1, t2)
getPi pos msg other = throwError $ displayPos pos ++ ": " ++ msg ++ "; Function expected:\n" ++ pp other
