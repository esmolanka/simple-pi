{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.SimplePi.LexerInterface
  ( LineCol(..)
  , AlexInput(..)
  , mkAlexInput
  -- Alex interfare
  , alexInputPrevChar
  , alexGetByte
  ) where

import Control.Applicative ((<|>))
import Data.Char
import Data.Maybe
import qualified Data.Text.Lazy as TL
import Data.Word (Word8)

data LineCol = LineCol {-# UNPACK #-} !Int {-# UNPACK #-} !Int

columnsInTab :: Int
columnsInTab = 8

advanceLineCol :: Char -> LineCol -> LineCol
advanceLineCol '\n' (LineCol line _)   = LineCol (line + 1) 1
advanceLineCol '\t' (LineCol line col) = LineCol line (col + columnsInTab)
advanceLineCol _    (LineCol line col) = LineCol line (col + 1)

data AlexInput = AlexInput
  { aiInput    :: TL.Text
  , aiPrevChar :: {-# UNPACK #-} !Char
  , aiLineCol  :: !LineCol
  }

mkAlexInput :: TL.Text -> AlexInput
mkAlexInput source = AlexInput
  { aiInput    = stripBOM source
  , aiPrevChar = '\n'
  , aiLineCol  = initPos
  }
  where
    initPos :: LineCol
    initPos = LineCol 1 1
    stripBOM :: TL.Text -> TL.Text
    stripBOM xs =
      fromMaybe xs $
      TL.stripPrefix utf8BOM xs <|> TL.stripPrefix utf8BOM' xs
    utf8BOM  = "\xFFEF"
    utf8BOM' = "\xFEFF"

-- Alex interface - functions usedby Alex
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = aiPrevChar

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input@AlexInput {aiInput, aiLineCol} =
  case TL.uncons aiInput of
    Nothing      -> Nothing
    Just (c, cs) -> Just $ encode c cs
  where
    encode :: Char -> TL.Text -> (Word8, AlexInput)
    encode c cs = (fixChar c, input')
      where
        input' :: AlexInput
        input' = input
          { aiInput    = cs
          , aiPrevChar = c
          , aiLineCol  = advanceLineCol c aiLineCol
          }

-- Translate unicode character into special symbol we taught Alex to recognize.
fixChar :: Char -> Word8
fixChar c
  -- Plain ascii case
  | c <= '\x7f' = fromIntegral $ ord $ c

  -- Unicode lambda
  | c == '\955'  =
      0x03

  -- Unicode forall
  | c == '\8704' =
      0x04

  -- Unicode arrow
  | c == '\8594' =
      0x05

  -- Unicode double arrow
  | c == '\8658' =
      0x06

  | generalCategory c >= UppercaseLetter &&
    generalCategory c <= OtherLetter =
      0x07

  -- Unicode spaces, separators, etc.
  | generalCategory c == Space =
      0x01

  -- Other unicode graphical
  | otherwise =
      0x02
