{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Language.SimplePi.Token where

import Data.Text (Text)
import Data.Text.Lazy (pack, fromStrict)
import Text.PrettyPrint.Leijen.Text

data Token
  = TokPunct      Text
  | TokReserved   Text
  | TokIdentifier { getIdent :: Text }
  | TokNumber     { getNum   :: Integer }
  | TokUnknown    Char
    deriving (Show, Eq)

data LocatedBy p a = L !p !a
  deriving (Show, Eq, Functor)

{-# INLINE mapPosition #-}
mapPosition :: (p -> p') -> LocatedBy p a -> LocatedBy p' a
mapPosition f (L p a) = L (f p) a

extract :: LocatedBy p a -> a
extract (L _ a) = a

position :: LocatedBy p a -> p
position (L p _) = p

infix 9 @@

(@@) :: a -> LocatedBy p b -> LocatedBy p a
(@@) a (L p _) = (L p a)

instance Pretty Token where
  pretty (TokPunct      s) = "punctuation" <+> squotes (text (fromStrict s))
  pretty (TokReserved   s) = "reserved" <+> dquotes (text (fromStrict s))
  pretty (TokIdentifier s) = "identifier" <+> dquotes (text (fromStrict s))
  pretty (TokNumber     n) = "number" <+> integer n
  pretty (TokUnknown    u) = "unknown lexeme" <+> text (pack (show u))
