module Main where

import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Except

import Data.Text.Lazy (unpack)

import Language.SexpGrammar (encodePrettyWith)

import SPI.Env
import SPI.Expr
import SPI.Typecheck

import SPI.Sugar (sugar)
import SPI.Grammar (sugaredGrammar)

app :: Expr -> Expr -> Expr
app f a = Fix $ App dummyPos f a

lambda :: String -> Maybe Expr -> Expr -> Expr
lambda arg mtyp body = Fix $ Lambda dummyPos (VarStr arg) mtyp body

forall :: String -> Expr -> Expr -> Expr
forall var a b = Fix $ Pi dummyPos (VarStr var) a b

infixr 2 ~>
(~>) :: Expr -> Expr -> Expr
(~>) a b = Fix $ Pi dummyPos Dummy a b

infix 9 .:
(.:) :: Expr -> Expr -> Expr
(.:) expr typ = Fix $ Annot dummyPos expr typ

var :: String -> Expr
var name = Fix $ Var dummyPos $ VarStr name

typ :: Expr
typ = Fix $ Universe dummyPos 0

typn :: Int -> Expr
typn = Fix . Universe dummyPos

infer :: Expr -> Expr
infer expr =
  let (res, _) = runState (runReaderT (runExceptT (inferType expr)) freshEnv) 0
  in either (error . ("\n" ++)) id res

prn :: Expr -> IO ()
prn =
  either error (putStrLn . unpack) .
    encodePrettyWith sugaredGrammar . sugar

test_inferpi1 :: Expr
test_inferpi1 =
    ( lambda "A" Nothing $
        lambda "f" Nothing $
          lambda "x" Nothing $
            app (var "f") (var "x")
    ) .:
    ( forall "X" typ $
        (var "X" ~> var "X") ~> var "X" ~> var "X"
    )

main :: IO ()
main = do
  prn $ infer $ test_inferpi1
