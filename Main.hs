{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Except

import qualified Data.Text.Lazy.IO as T
import Data.Text.Lazy (Text, pack)

import Data.Monoid ((<>))

import SimpTT.Sugar
import SimpTT.Expr (Variable (..), universe)
import qualified SimpTT.Eval as Eval
import qualified SimpTT.Context as Ctx

import Language.SimplePi

type EvalT m = ExceptT String (StateT Eval.Ctx m)

eval :: (Monad m) => ExceptT Eval.Err (Reader Eval.Ctx) a -> EvalT m a
eval act = Eval.runEval act =<< get

processStatement :: Statement -> EvalT IO (Maybe Text)
processStatement stmt =
  case stmt of
    (Load filename) -> do
      prog <- liftIO (T.readFile filename)
      stmts <- either throwError return $ parseProgram filename prog
      mapM_ processStatement stmts
      return Nothing

    (Definition idn expr) -> do
      let var = desugarIdent idn
          expr' = desugar expr
      ty <- eval (Eval.inferType expr')
      modify (Ctx.extendDef var (Ctx.Entry ty (Just expr')))
      return Nothing

    (Parameter idn ty) -> do
      let var = desugarIdent idn
      modify (Ctx.extend var (desugar ty))
      return Nothing
    (Check expr) -> do
      expr' <- eval (Eval.inferType $ desugar expr)
      return (Just (prettyExpr $ sugar expr'))
    (Eval expr) -> do
      ty <- eval (Eval.inferType $ desugar expr)
      expr' <- eval (Eval.normalize $ desugar expr)
      return (Just $ prettyExpr (sugar expr') <> pack "\n: " <> prettyExpr (sugar ty))

evalProg :: String -> EvalT IO ()
evalProg input = do
  stmts <- either throwError return $ parseProgram "stdin" (pack input)
  forM_ stmts $ \stmt -> do
    response <- processStatement stmt
    case response of
      Nothing -> return ()
      Just a  -> liftIO $ T.putStrLn a

freshCtx :: Eval.Ctx
freshCtx =
  Ctx.extendDef (Variable "Type₃") (Ctx.Entry (universe 4) (Just $ universe 3)) $
  Ctx.extendDef (Variable "Type₂") (Ctx.Entry (universe 3) (Just $ universe 2)) $
  Ctx.extendDef (Variable "Type₁") (Ctx.Entry (universe 2) (Just $ universe 1)) $
  Ctx.extendDef (Variable "Type")  (Ctx.Entry (universe 1) (Just $ universe 0)) $
  Ctx.empty

main :: IO ()
main = do
  prog <- getContents
  res <- evalStateT (runExceptT (evalProg prog)) freshCtx
  case res of
    Left err -> putStrLn err
    Right () -> return ()
