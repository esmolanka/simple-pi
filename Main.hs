{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

-- import System.Console.Haskeline

import Control.Arrow (first)
import Control.Monad.State.Strict
import Control.Monad.Except

import Data.ByteString.Lazy.Char8 (pack, unpack)

import Language.Sexp (parseSexps)
import Language.SexpGrammar

import Expr
import Sugar
import Grammar

type EvalT m = ExceptT String (StateT (Context, Int) m)

eval :: (Monad m) => (Context -> ExceptT String (State Int) a) -> EvalT m a
eval f = do
  (ctx, n) <- get
  let (res, n') = runState (runExceptT (f ctx)) n
  put (ctx, n')
  either throwError return res


showExpr :: (Monad m) => Expr -> EvalT m String
showExpr =
  either throwError (return . unpack) .
    encodePrettyWith expressionGrammar

processStatement :: (Monad m) => Statement -> EvalT m (Maybe String)
processStatement stmt =
  case stmt of
    (Definition var expr) -> do
      ty <- eval (`inferType` desugar expr)
      modify (first (extend var ty (Just (desugar expr))))
      return Nothing
    (Parameter var ty) -> do
      modify (first (extend var (desugar ty) Nothing))
      return Nothing
    (Check expr) -> do
      expr' <- eval (`inferType` desugar expr)
      fmap Just (showExpr expr')
    (Eval expr) -> do
      expr' <- eval (`normalize` desugar expr)
      fmap Just (showExpr expr')
    (AssertType expr ty) -> do
      eval $ \ctx -> do
        ty' <- inferType ctx (desugar expr)
        checkEqual ctx (desugar ty) ty'
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
  res <- evalStateT (runExceptT (evalProg prog)) (fresh, 0)
  case res of
    Left err -> error err
    Right () -> return ()
