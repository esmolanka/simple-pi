{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveFunctor       #-}

module SPI.Expr
  ( Expr
  , ExprF (..)
  , Variable (..)
  , getPos
  , dummyPos
  , Position
  , Fix (..)
  , unFix
  ) where

import Data.Functor.Foldable (Fix (..))
import GHC.Generics (Generic)
import Language.Sexp (Position, dummyPos)
import Data.Coerce

data Variable
  = VarStr String
  | GenSym String Int
  | Dummy
    deriving (Show, Eq, Ord, Generic)

type Expr = Fix ExprF

data ExprF e
  = Var      Position Variable
  | Universe Position Int
  | Pi       Position Variable e e
  | Lambda   Position Variable e e
  | App      Position e e
    deriving (Show, Eq, Ord, Functor, Generic)

getPos :: Expr -> Position
getPos (Fix (Var pos _)) = pos
getPos (Fix (Universe pos _)) = pos
getPos (Fix (Pi pos _ _ _)) = pos
getPos (Fix (Lambda pos _ _ _)) = pos
getPos (Fix (App pos _ _)) = pos

----------------------------------------------------------------------
-- Utils

unFix :: Fix f -> f (Fix f)
unFix = coerce
