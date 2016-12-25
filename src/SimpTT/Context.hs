{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleInstances   #-}

module SimpTT.Context
  ( Entry (..)
  , Context (..)
  , empty
  , lookup
  , extendDef
  , extend
  ) where

import Prelude hiding (lookup)
import Data.Map (Map)
import qualified Data.Map as M
import Text.PrettyPrint.Leijen.Text hiding (empty)
import Data.Text.Lazy (fromStrict)

import SimpTT.Expr

data Entry e = Entry
  { entryType :: e
  , entryTerm :: Maybe e
  } deriving (Functor, Foldable, Traversable)

newtype Context e = Context (Map Variable [Entry e])
  deriving (Functor, Foldable, Traversable)

instance Pretty (Context Doc) where
  pretty (Context ctx) =
    vsep $ concatMap
      (\(var, entries) -> zipWith (ppEntry var) [0..] entries)
      (M.toAscList ctx)
    where
      ppEntry :: Variable -> Integer -> Entry Doc -> Doc
      ppEntry var idx (Entry typ Nothing) =
        fillBreak 10 (ppVar var idx) <+> colon <+> typ
      ppEntry var idx (Entry typ (Just term)) =
        vcat [ fillBreak 10 (ppVar var idx) <+> colon <+> typ
             , indent 11 $ equals <+> term
             ]

      ppVar (Variable x) 0 = text (fromStrict x)
      ppVar (Variable x) n = pretty (fromStrict x) <> pretty "/" <> pretty n

empty :: Context e
empty = Context M.empty

lookup :: Variable -> Integer -> Context e -> Maybe (Entry e)
lookup v n (Context c) =
  case snd . splitAt (fromInteger n) . M.findWithDefault [] v $ c of
    [] -> Nothing
    (x : _) -> Just x

extendDef :: Variable -> Entry Expr -> Context Expr -> Context Expr
extendDef x e (Context c) =
  Context $
    M.alter (addEntry e) x $
      M.map (map (shiftEntry 1 x)) c
  where
    addEntry :: Entry Expr -> Maybe [Entry Expr] -> Maybe [Entry Expr]
    addEntry e Nothing = Just [e]
    addEntry e (Just es) = Just (e : es)

    shiftEntry :: Integer -> Variable -> Entry Expr -> Entry Expr
    shiftEntry n x (Entry typ term) =
      Entry (shift n x typ) (fmap (shift n x) term)

extend :: Variable -> Expr -> Context Expr -> Context Expr
extend x t = extendDef x (Entry t Nothing)
