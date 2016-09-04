module SPI.Pretty
  ( displayExpr
  , displayPos
  ) where

import qualified Data.Text.Lazy as T
import Data.List (intercalate)

import Language.SexpGrammar (encodePrettyWith)

import SPI.Expr
import SPI.Grammar
import SPI.Sugar

import Language.SimplePi.Types (Position (..))

displayExpr :: Expr -> String
displayExpr =
  either ("printing error: " ++) (T.unpack) .
    encodePrettyWith sugaredGrammar . sugar

displayPos :: Position -> String
displayPos (Position file line col) =
  intercalate ":" [ file, show line, show col ]
