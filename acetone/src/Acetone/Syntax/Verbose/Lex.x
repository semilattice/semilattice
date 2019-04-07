{
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Acetone.Syntax.Verbose.Lex where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
}

-- TODO: Return Either instead of crashing.

%wrapper "posn-bytestring"

tokens :-

  [\ \n]                  ;

  "abstract"              { \p _ -> (p, KeywordT "abstract") }
  "all"                   { \p _ -> (p, KeywordT "all") }
  "calculus"              { \p _ -> (p, KeywordT "calculus") }
  "division"              { \p _ -> (p, KeywordT "division") }
  "end-value"             { \p _ -> (p, KeywordT "end-value") }
  "external"              { \p _ -> (p, KeywordT "external") }
  "for"                   { \p _ -> (p, KeywordT "for") }
  "identification"        { \p _ -> (p, KeywordT "identification") }
  "interface"             { \p _ -> (p, KeywordT "interface") }
  "is"                    { \p _ -> (p, KeywordT "is") }
  "linkage"               { \p _ -> (p, KeywordT "linkage") }
  "over"                  { \p _ -> (p, KeywordT "over") }
  "signature"             { \p _ -> (p, KeywordT "signature") }
  "such"                  { \p _ -> (p, KeywordT "such") }
  "that"                  { \p _ -> (p, KeywordT "that") }
  "value-id"              { \p _ -> (p, KeywordT "value-id") }

  "("                     { \p _ -> (p, PunctuationT "(") }
  ")"                     { \p _ -> (p, PunctuationT ")") }
  "->"                    { \p _ -> (p, PunctuationT "->") }
  "."                     { \p _ -> (p, PunctuationT ".") }

  [A-Za-z\-]+             { \p s -> (p, IdentifierT (toStrict s)) }

{
data Token :: * where
  KeywordT :: ByteString -> Token
  PunctuationT :: ByteString -> Token
  IdentifierT :: ByteString -> Token
  deriving stock (Eq, Show)
}
