{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.SimplePi.Pretty where

-- import Data.Text.Lazy (Text, pack)
import Text.PrettyPrint.Leijen.Text
import Language.SimplePi.Types

instance Pretty a => Pretty (ExprF a) where
  pretty _ = undefined
