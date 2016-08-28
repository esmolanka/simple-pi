{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Control.Arrow (first)
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Except

import Data.Text.Lazy (pack, unpack)

import Language.Sexp (parseSexps)
import Language.SexpGrammar

import SPI.Sugar
import SPI.Expr
import SPI.Typecheck
import SPI.Grammar
import SPI.Env

type EvalT m = ExceptT String (StateT (Context, Int) m)

eval :: (Monad m) => (ExceptT String (ReaderT Env (State Int)) a) -> EvalT m a
eval f = do
  (ctx, n) <- get
  let (res, n') = runState (runReaderT (runExceptT f) (Env { gamma = ctx, annotation = Any })) n
  put (ctx, n')
  either throwError return res

showExpr :: (Monad m) => Expr -> EvalT m String
showExpr =
  either throwError (return . unpack) .
    encodePrettyWith sugaredGrammar . sugar

processStatement :: Statement -> EvalT IO (Maybe String)
processStatement stmt =
  case stmt of
    (Load filename) -> do
      prog <- liftIO (readFile filename)
      sexps <- either throwError return $ parseSexps filename (pack prog)
      forM_ sexps $ \s -> do
        stmt <- either throwError return (parseSexp statementGrammar s)
        processStatement stmt
      return Nothing
    (Definition var expr mtype) -> do
      let expr' = maybe expr (\ty -> Fix $ SAnnot dummyPos expr ty) mtype
      ty <- eval (inferType $ desugar expr')
      modify (first (extendCtx var ty (Just (desugar expr))))
      return Nothing
    (Parameter var ty) -> do
      modify (first (extendCtx var (desugar ty) Nothing))
      return Nothing
    (Check expr) -> do
      expr' <- eval (inferType $ desugar expr)
      fmap Just (showExpr expr')
    (Eval expr) -> do
      expr' <- eval (normalize $ desugar expr)
      fmap Just (showExpr expr')
    (AssertType expr ty) -> do
      eval $ do
        ty' <- inferType (desugar expr)
        checkEqual (desugar ty) ty'
      return Nothing

evalProg :: String -> EvalT IO ()
evalProg input = do
  sexps <- either throwError return $ parseSexps "<stdin>" (pack input)
  forM_ sexps $ \s -> do
    stmt <- either throwError return (parseSexp statementGrammar s)
    response <- processStatement stmt
    case response of
      Nothing -> return ()
      Just a  -> liftIO $ putStrLn a

main :: IO ()
main = do
  prog <- getContents
  res <- evalStateT (runExceptT (evalProg prog)) (freshCtx, 0)
  case res of
    Left err -> putStrLn err
    Right () -> return ()
