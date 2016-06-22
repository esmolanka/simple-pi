
module SPI.Pretty
  ( displayExpr
  , displayPos
  ) where

import qualified Data.ByteString.Lazy.Char8 as B8
import Data.List (intercalate)

import Language.Sexp (Position (..))
import Language.SexpGrammar (encodePrettyWith)

import SPI.Expr
import SPI.Grammar
import SPI.Sugar

displayExpr :: Expr -> String
displayExpr =
  either ("printing error: " ++) (B8.unpack) .
    encodePrettyWith sugaredGrammar . sugar

displayPos :: Position -> String
displayPos (Position file line col) =
  intercalate ":" [ file, show line, show col ]
