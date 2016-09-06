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

import qualified Data.Text.Lazy.IO as T
import Data.Text.Lazy (Text, pack)

import Data.Monoid ((<>))

import SPI.Sugar
import qualified SPI.Expr as Internal
import SPI.Typecheck
import SPI.Env

import Language.SimplePi

type EvalT m = ExceptT String (StateT (Context, Int) m)

eval :: (Monad m) => (ExceptT String (ReaderT Env (State Int)) a) -> EvalT m a
eval f = do
  (ctx, n) <- get
  let (res, n') = runState (runReaderT (runExceptT f) (Env { gamma = ctx, annotation = Any })) n
  put (ctx, n')
  either throwError return res

processStatement :: Statement -> EvalT IO (Maybe Text)
processStatement stmt =
  case stmt of
    (Load filename) -> do
      prog <- liftIO (T.readFile filename)
      stmts <- either throwError return $ parseProgram filename prog
      mapM_ processStatement stmts
      return Nothing
    (Definition idn expr) -> do
      mtype <- gets (lookupType (desugarIdent idn) . fst)
      let var = desugarIdent idn
          expr' = maybe (desugar expr) (\ty -> Fix $ Internal.Annot dummyPos (desugar expr) ty) mtype
      ty <- eval (inferType expr')
      modify (first (extendCtx var ty (Just expr')))
      return Nothing
    (Parameter idn ty) -> do
      let var = desugarIdent idn
      modify (first (extendCtx var (desugar ty) Nothing))
      return Nothing
    (Check expr) -> do
      expr' <- eval (inferType $ desugar expr)
      return (Just (prettyExpr $ sugar expr'))
    (Eval expr) -> do
      ty <- eval (inferType $ desugar expr)
      expr' <- eval (normalize $ desugar expr)
      return (Just $ prettyExpr (sugar expr') <> pack "\n: " <> prettyExpr (sugar ty))

evalProg :: String -> EvalT IO ()
evalProg input = do
  stmts <- either throwError return $ parseProgram "stdin" (pack input)
  forM_ stmts $ \stmt -> do
    response <- processStatement stmt
    case response of
      Nothing -> return ()
      Just a  -> liftIO $ T.putStrLn a

main :: IO ()
main = do
  prog <- getContents
  res <- evalStateT (runExceptT (evalProg prog)) (freshCtx, 0)
  case res of
    Left err -> putStrLn err
    Right () -> return ()
