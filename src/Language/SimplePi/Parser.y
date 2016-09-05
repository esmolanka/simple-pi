{
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing      #-}
{-# OPTIONS_GHC -fno-warn-tabs                #-}
{-# OPTIONS_GHC -fno-warn-unused-binds        #-}
{-# OPTIONS_GHC -fno-warn-unused-matches      #-}

module Language.SimplePi.Parser
  ( pExpression
  , pStatement
  , pProgram
  ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as Lazy

import Text.PrettyPrint.Leijen.Text

import Language.SimplePi.Token
import Language.SimplePi.Lexer
import Language.SimplePi.Types
}

%name pProgram    Program
%name pStatement  Statement
%name pExpression Expression
%error { parseError }
%tokentype { LocatedBy Position Token }
%monad { Either String }

%expect 3

%token
  '('            { L _ (TokPunct "("   ) }
  ')'            { L _ (TokPunct ")"   ) }
  ':'            { L _ (TokPunct ":"   ) }
  '->'           { L _ (TokPunct "->"  ) }
  '=>'           { L _ (TokPunct "=>"  ) }
  '\\'           { L _ (TokPunct "\\"  ) }
  '='            { L _ (TokPunct "="   ) }
  '.'            { L _ (TokPunct "."   ) }
  ident          { L _ (TokIdentifier _) }
  num            { L _ (TokNumber     _) }

  "Load"         { L _ (TokReserved "Load" ) }
  "Check"        { L _ (TokReserved "Check") }
  "Eval"         { L _ (TokReserved "Eval" ) }

%left ':'
%right '=>'
%right '->'

%%

-- Program

Program :: { [Statement] }
  : list(Statement)                      { $1 }

-- Statement

Statement :: { Statement }
  : "Load" ident '.'                     { Load $ mkFileName $ getIdent $ extract $2 }
  | ident ':' Expression '.'             { Parameter (Ident $ getIdent $ extract $1) $3 }
  | ident '=' Expression '.'             { Definition (Ident $ getIdent $ extract $1) $3 }
  | "Check" Expression '.'               { Check $2 }
  | "Eval" Expression '.'                { Eval $2 }

-- Expression

Expression :: { Expr }
  : AppExpr                              { $1 }
  | '\\' list1(LamBnd) '=>' Expression   { Fix $ Lambda (position $1) (concat $2) $4 }
  | PiBnd '->' Expression                { Fix $ Pi (position $1) (extract $1) $3 }
  | AppExpr '->' sepBy1(Expression, '->') { mkArrow (position $2) ($1 : $3) }

LamBnd :: { [ Binding Maybe Expr ] }
  : '(' list1(ident) ':' Expression ')'  { map (\idn -> Binding (Ident $ getIdent $ extract idn) (Just $4)) $2 }
  | ident                                { [ Binding (Ident $ getIdent $ extract $1) Nothing ] }

PiBnd :: { Located (Binding Identity Expr) }
  : '(' ident ':' Expression ')'         { Binding (Ident $ getIdent $ extract $2) (Identity $4) @@ $1 }

AppExpr :: { Expr }
  : list1(AtomExpr)                      { mkApplication $1 }

AtomExpr :: { Expr }
  : num                                  { Fix $ Universe (position $1) (getNum $ extract $1) }
  | ident                                { Fix $ Var      (position $1) (Ident $ getIdent $ extract $1) }
  | '(' Expression ')'                   { $2 }

-- Utils

rev_list1(p)
  : p                      { [$1]    }
  | rev_list1(p) p         { $2 : $1 }

list1(p)
  : rev_list1(p)           { reverse $1 }

list(p)
  : {- empty -}            { [] }
  | list1(p)               { $1 }

fst(p, q)
  : p q                    { $1 }

snd(p, q)
  : p q                    { $2 }

both(p, q)
  : p q                    { ($1, $2) }

sepBy1(p, q)
  : p list(snd(q, p))      { $1 : $2 }

sepBy2(p, q)
  : p q sepBy1(p, q)       { $1 : $3 }

{
--

mkArrow :: Position -> [Expr] -> Expr
mkArrow pos (a : b : rest) = Fix $ Arrow pos a b rest
mkArrow pos _ = error "Unexpected input on mkArrow"

mkApplication :: [Expr] -> Expr
mkApplication (a : b : rest) = Fix $ App (getPosition a) a b rest
mkApplication (a : []) = a
mkApplication _ = error "Unexpected input on mkApplication"

mkFileName :: T.Text -> FilePath
mkFileName t = T.unpack t ++ ".spi"

type Located = LocatedBy Position

parseError :: [LocatedBy Position Token] -> Either String b
parseError toks = case toks of
  [] ->
    Left "EOF: Unexpected end of file"
  (L pos tok : _) ->
    Left $ Lazy.unpack . displayT . renderPretty 0.8 80 $
      pretty pos <> colon <+> "Unexpected token:" <+> pretty tok

}
