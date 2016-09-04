{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveGeneric     #-}

module Language.SimplePi.Types
  ( Position (..)
  , dummyPos
  , Statement (..)
  , Expr
  , Ident (..)
  , Binding (..)
  , ExprF (..)
  , getPosition
  , Identity (..)
  , Fix (..)
  ) where

import Data.Text
import Data.Functor.Identity
import Data.Functor.Foldable (Fix (..))
import Text.PrettyPrint.Leijen.Text

import GHC.Generics

-- Statement

data Statement
  = Load       FilePath
  | Parameter  Ident Expr {- <var> : <expr> -}
  | Definition Ident Expr {- <var> = <expr> -}
  | Check      Expr
  | Eval       Expr
    deriving (Show, Eq, Generic)

-- Expression

type Expr = Fix ExprF

newtype Ident = Ident Text
  deriving (Show, Eq, Ord, Generic)

data Binding f e = Binding Ident (f e)
  deriving (Show, Eq, Foldable, Functor, Generic)

data ExprF e
  = Lambda   Position [Binding Maybe e] e
  | Pi       Position (Binding Identity e) e
  | Arrow    Position e e [e]
  | App      Position e e [e]
  | Annot    Position e e
  | Var      Position Ident
  | Universe Position Integer
    deriving (Show, Eq, Functor, Foldable, Generic)

getPosition :: Expr -> Position
getPosition (Fix e) =
  case e of
    (Lambda pos _ _) -> pos
    (Pi     pos _ _) -> pos
    (Arrow  pos _ _ _) -> pos
    (App    pos _ _ _) -> pos
    (Annot  pos _ _) -> pos
    (Var    pos _) -> pos
    (Universe pos _) -> pos


-- | File position
data Position = Position
  { posFileName :: !FilePath
  , posLine     :: {-# UNPACK #-} !Int
  , posColumn   :: {-# UNPACK #-} !Int
  } deriving (Show, Ord, Eq)

dummyPos :: Position
dummyPos = Position "<no location information>" 1 0

instance Pretty Position where
  pretty = pretty . show
