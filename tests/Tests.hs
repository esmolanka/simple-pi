{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.Lazy (unpack)

import SimpTT.Expr
import SimpTT.Eval
import SimpTT.Sugar
import qualified SimpTT.Context as Ctx

import Language.SimplePi (prettyExpr)

infer :: Expr -> Expr
infer expr =
  let res = runEval (inferType expr) Ctx.empty
  in either error id res

prn :: Expr -> IO ()
prn = putStrLn . unpack . prettyExpr . sugar

-- test_inferpi1 :: Expr
-- test_inferpi1 =
--   app
--   ( app (var "the" 0)
--     ( forall "X" typ $
--         (var "X" 0 ~> var "X" 0) ~> var "X" 0 ~> var "X" 0
--     ))
--     ( lambda "A" Nothing $
--         lambda "f" Nothing $
--           lambda "x" Nothing $
--             app (var "f" 0) (var "x" 0)
--     )

main :: IO ()
main = do
  prn $
    subst (Variable "x") 0 (var "foo" 0) $
      app (lam "x" (universe 0) (var "x" 0)) (var "x" 0)

  -- prn $ infer $ test_inferpi1
