{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}

module SimpTT.Error where

import Language.SimplePi.Types (Position)
import Text.PrettyPrint.Leijen.Text

data TTError ctx e
  = VariableNotFound Position e (ctx e)
  | TypeExpected Position e e
  | FunctionExpected Position e e
  | TypeMismatch Position String e e e
  deriving (Show, Functor, Foldable, Traversable)

instance (Pretty e, Pretty (ctx e)) => Pretty (TTError ctx e) where
  pretty (VariableNotFound pos var ctx) =
    vsep [ pretty pos <> colon <+> "Variable not found:"
         , indent 2 $ align $ pretty var
         , ""
         , "In context:"
         , indent 2 $ align $ pretty ctx
         ]
  pretty (TypeExpected pos term typ) =
    vsep [ pretty pos <> colon <+> "Type expected, but got:"
         , ""
         , "During type checking:"
         , indent 2 $ align $ pretty term
         , ""
         , "Got type:"
         , indent 2 $ align $ pretty typ
         ]
  pretty (FunctionExpected pos term typ) =
    vsep [ pretty pos <> colon <+> "Function expected."
         , ""
         , "During type checking:"
         , indent 2 $ align $ pretty term
         , ""
         , "Got type:"
         , indent 2 $ align $ pretty typ
         ]
  pretty (TypeMismatch pos msg term expected got) =
    vsep [ pretty pos <> colon <+> pretty msg
         , ""
         , "During type checking:"
         , indent 2 $ align $ pretty term
         , ""
         , "Expected type:"
         , indent 2 $ align $ pretty expected
         , ""
         , "Got type:"
         , indent 2 $ align $ pretty got
         ]
