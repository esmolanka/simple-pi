module Language.SimplePi
  ( module Language.SimplePi.Types
  , parseExpr
  , prettyExpr
  , parseStatement
  , parseProgram
  ) where

import Data.Text.Lazy (Text)
import qualified Data.Text as T

import Text.PrettyPrint.Leijen.Text

import Language.SimplePi.Types
import Language.SimplePi.Lexer
import Language.SimplePi.Parser

import Language.SimplePi.Pretty (ppExpr)

parseExpr :: FilePath -> Text -> Either String Expr
parseExpr fn str = pExpression (lexSimplePi (dummyPos { posFileName = T.pack fn }) str)

parseStatement :: FilePath -> Text -> Either String Statement
parseStatement fn str = pStatement (lexSimplePi (dummyPos { posFileName = T.pack fn }) str)

parseProgram :: FilePath -> Text -> Either String [Statement]
parseProgram fn str = pProgram (lexSimplePi (dummyPos { posFileName = T.pack fn }) str)

prettyExpr :: Expr -> Text
prettyExpr = displayT . renderPretty 1.0 100 . ppExpr 0
