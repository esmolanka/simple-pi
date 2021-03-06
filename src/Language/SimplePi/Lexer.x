{
{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing     #-}
{-# OPTIONS_GHC -fno-warn-tabs               #-}
{-# OPTIONS_GHC -fno-warn-unused-binds       #-}
{-# OPTIONS_GHC -fno-warn-unused-imports     #-}
{-# OPTIONS_GHC -fno-warn-unused-matches     #-}

module Language.SimplePi.Lexer
  ( lexSimplePi
  ) where

import qualified Data.Text as T
import Data.Text.Read
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as B8

import Language.SimplePi.LexerInterface
import Language.SimplePi.Token
import Language.SimplePi.Types (Position (..))
}

$whitechar   = [\ \t\n\r\f\v]
$unispace    = \x01
$whitespace  = [$whitechar $unispace]

$unialpha    = \x07

$uninonspace = \x02
$uniany      = [$unialpha $unispace $uninonspace]
@any         = (. | $uniany)

$lambda      = \x03
$forall      = \x04
$arrow       = \x05
$darrow      = \x06

$digit       = 0-9
$alpha       = [a-z A-Z]

@natural     = $digit+

$idinitial   = [$alpha $unialpha \_]
$idsubseq    = [$idinitial $digit $uninonspace \- \']
@identifier  = $idinitial $idsubseq*

@lambda      = [\\ $lambda]
@forall      = "forall" | [$forall]
@arrow       = "->" | [$arrow]
@darrow      = "=>" | "." | [$darrow]

:-

$whitespace+              ;
$whitespace ^ "--" @any*  ;

"("                { TokPunct      `via` id }
")"                { TokPunct      `via` id }
":"                { TokPunct      `via` id }
@lambda            { just TokLambda         }
@forall            { just TokForall         }
@arrow             { just TokArrow          }
@darrow            { just TokDblArrow       }
"="                { TokPunct      `via` id }
","                { TokPunct      `via` id }

"Load"             { TokReserved   `via` id }
"Eval"             { TokReserved   `via` id }
"Check"            { TokReserved   `via` id }

@natural           { TokNumber     `via` readInteger }
@identifier        { TokIdentifier `via` id          }

@any               { TokUnknown    `via` T.head      }

{

type AlexAction = LineCol -> TL.Text -> LocatedBy LineCol Token

readInteger :: T.Text -> Integer
readInteger str =
  case signed decimal str of
    Left err -> error $ "Lexer is broken: " ++ err
    Right (a, rest)
      | T.null (T.strip rest) -> a
      | otherwise -> error $ "Lexer is broken, leftover: " ++ show rest

just :: Token -> AlexAction
just tok pos _ =
  L pos tok

via :: (a -> Token) -> (T.Text -> a) -> AlexAction
via ftok f pos str =
  L pos . ftok . f . TL.toStrict $str

alexScanTokens :: AlexInput -> [LocatedBy LineCol Token]
alexScanTokens input =
  case alexScan input defaultCode of
    AlexEOF -> []
    AlexError (AlexInput {aiInput, aiLineCol = LineCol line col}) ->
      error $ "Lexical error at line " ++ show line ++ " column " ++ show col ++
              ". Remaining input: " ++ TL.unpack (TL.take 1000 aiInput)
    AlexSkip input _ -> alexScanTokens input
    AlexToken input' tokLen action ->
      let pos@(LineCol _ col) = aiLineCol input
          tokens = action pos inputText : alexScanTokens input'
      in if col == 1
         then L pos TokNewline : tokens
         else tokens
      where
        -- It is safe to take token length from input because every byte Alex
        -- sees corresponds to exactly one character, even if character is a
        -- Unicode one that occupies several bytes. We do character translation
        -- in LexerInterface.alexGetByte function so that all unicode characters
        -- occupy single byte.
        --
        -- On the other hand, taking N characters from Text will take N valid
        -- characters, not N bytes.
        --
        -- Thus, we're good.
        inputText = TL.take (fromIntegral tokLen) $ aiInput input
  where
    defaultCode :: Int
    defaultCode = 0

lexSimplePi :: Position -> TL.Text -> [LocatedBy Position Token]
lexSimplePi (Position fn line1 col1) =
  map (mapPosition fixPos) . alexScanTokens . mkAlexInput
  where
    fixPos (LineCol l c) | l == 1    = Position fn line1 (col1 + c)
                         | otherwise = Position fn (pred l + line1) c

}
