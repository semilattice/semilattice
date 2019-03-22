{-# LANGUAGE OverloadedStrings #-}

module Salp.Lex
  ( -- * Parser
    Parser

    -- * Tokens
  , commaT
  , leftBraceT
  , leftParenthesisT
  , periodT
  , rightBraceT
  , rightParenthesisT
  , semicolonT

    -- * Words
  , identifierT
  , keywords
  , classK
  , fromK
  , importK
  , namespaceK
  , programK
  , recordK
  , returnK
  , rootK
  , stringK
  , versionK

    -- * Miscellaneous
  , whitespace
  , token
  , keyword
  , isWhitespace
  , isIdentifier
  ) where

import Control.Applicative (many)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Functor (void)
import Data.Functor.Identity (Identity)
import Data.HashSet (HashSet)
import Data.Void (Void)
import Data.Word (Word8)

import qualified Data.HashSet as HashSet
import qualified Text.Megaparsec as A
import qualified Text.Megaparsec.Byte as A

--------------------------------------------------------------------------------
-- Parser

type Parser =
  A.ParsecT Void ByteString Identity

--------------------------------------------------------------------------------
-- Tokens

commaT :: Parser ()
commaT = punctuation ","

leftBraceT :: Parser ()
leftBraceT = punctuation "{"

leftParenthesisT :: Parser ()
leftParenthesisT = punctuation "("

periodT :: Parser ()
periodT = punctuation "."

rightBraceT :: Parser ()
rightBraceT = punctuation "}"

rightParenthesisT :: Parser ()
rightParenthesisT = punctuation ")"

semicolonT :: Parser ()
semicolonT = punctuation ";"

--------------------------------------------------------------------------------
-- Words

identifierT :: Parser ByteString
identifierT = token $ do
  name <- A.takeWhile1P Nothing isIdentifier
  when (name `HashSet.member` keywords) $
    fail "identifierT: keyword"
  pure name

keywords :: HashSet ByteString
keywords = HashSet.fromList
  [ "class", "from", "namespace"
  , "program", "record", "return"
  , "root", "string", "version" ]

classK :: Parser ()
classK = keyword "class"

fromK :: Parser ()
fromK = keyword "from"

importK :: Parser ()
importK = keyword "import"

namespaceK :: Parser ()
namespaceK = keyword "namespace"

programK :: Parser ()
programK = keyword "program"

recordK :: Parser ()
recordK = keyword "record"

returnK :: Parser ()
returnK = keyword "return"

rootK :: Parser ()
rootK = keyword "root"

stringK :: Parser ()
stringK = keyword "string"

versionK :: Parser ()
versionK = keyword "version"

--------------------------------------------------------------------------------
-- Miscellaneous

whitespace :: Parser ()
whitespace = void . many $ A.satisfy isWhitespace

token :: Parser a -> Parser a
token = A.try . (<* whitespace)

punctuation :: ByteString -> Parser ()
punctuation name = token $
  void $ A.string name

keyword :: ByteString -> Parser ()
keyword name = token $ do
  _ <- A.string name
  A.notFollowedBy $ A.satisfy isIdentifier
  pure ()

isWhitespace :: Word8 -> Bool
isWhitespace c =
  c == 0x20 || c == 0x0A

isIdentifier :: Word8 -> Bool
isIdentifier c =
  (c >= 0x41 && c <= 0x5A) ||
  (c >= 0x61 && c <= 0x7A)
