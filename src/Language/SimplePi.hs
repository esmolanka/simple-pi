module Language.SimplePi
  ( module Language.SimplePi.Types
  , parseExpr
  , prettyExpr
  , parseStatement
  , parseProgram
  ) where

import Data.Text.Lazy (Text, pack)

import Text.PrettyPrint.Leijen.Text

import Language.SimplePi.Types
import Language.SimplePi.Lexer
import Language.SimplePi.Parser
import Language.SimplePi.Pretty

parseExpr :: Text -> Either String Expr
parseExpr str = pExpression (lexSimplePi dummyPos str)

parseStatement :: Text -> Either String Statement
parseStatement str = pStatement (lexSimplePi dummyPos str)

parseProgram :: Text -> Either String [Statement]
parseProgram str = pProgram (lexSimplePi dummyPos str)

prettyExpr :: Expr -> Text
prettyExpr = pack . show
