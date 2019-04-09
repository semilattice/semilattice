{
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Acetone.Syntax.Verbose.Lex where

import Data.ByteString (ByteString)

import qualified Data.ByteString.Lazy as BSL
}

-- TODO: Return Either instead of crashing.

%wrapper "posn-bytestring"

tokens :-

  [\ \n]                  ;
  "*>".*                  ;

  "abstract"              { \p _ -> (p, KeywordT "abstract") }
  "all"                   { \p _ -> (p, KeywordT "all") }
  "calculus"              { \p _ -> (p, KeywordT "calculus") }
  "defer"                 { \p _ -> (p, KeywordT "defer") }
  "division"              { \p _ -> (p, KeywordT "division") }
  "element"               { \p _ -> (p, KeywordT "element") }
  "end-evaluate"          { \p _ -> (p, KeywordT "end-evaluate") }
  "end-record"            { \p _ -> (p, KeywordT "end-record") }
  "end-record-type"       { \p _ -> (p, KeywordT "end-record-type") }
  "end-value"             { \p _ -> (p, KeywordT "end-value") }
  "end-variant"           { \p _ -> (p, KeywordT "end-variant") }
  "end-variant-type"      { \p _ -> (p, KeywordT "end-variant-type") }
  "evaluate"              { \p _ -> (p, KeywordT "evaluate") }
  "external"              { \p _ -> (p, KeywordT "external") }
  "for"                   { \p _ -> (p, KeywordT "for") }
  "force"                 { \p _ -> (p, KeywordT "force") }
  "identification"        { \p _ -> (p, KeywordT "identification") }
  "interface"             { \p _ -> (p, KeywordT "interface") }
  "internal"              { \p _ -> (p, KeywordT "internal") }
  "is"                    { \p _ -> (p, KeywordT "is") }
  "linkage"               { \p _ -> (p, KeywordT "linkage") }
  "over"                  { \p _ -> (p, KeywordT "over") }
  "record"                { \p _ -> (p, KeywordT "record") }
  "record-type"           { \p _ -> (p, KeywordT "record-type") }
  "remaining"             { \p _ -> (p, KeywordT "remaining") }
  "since"                 { \p _ -> (p, KeywordT "since") }
  "signature"             { \p _ -> (p, KeywordT "signature") }
  "such"                  { \p _ -> (p, KeywordT "such") }
  "that"                  { \p _ -> (p, KeywordT "that") }
  "then"                  { \p _ -> (p, KeywordT "then") }
  "usage"                 { \p _ -> (p, KeywordT "usage") }
  "value-id"              { \p _ -> (p, KeywordT "value-id") }
  "variant"               { \p _ -> (p, KeywordT "variant") }
  "variant-type"          { \p _ -> (p, KeywordT "variant-type") }
  "when"                  { \p _ -> (p, KeywordT "when") }

  "#"                     { \p _ -> (p, PunctuationT "#") }
  "("                     { \p _ -> (p, PunctuationT "(") }
  ")"                     { \p _ -> (p, PunctuationT ")") }
  "->"                    { \p _ -> (p, PunctuationT "->") }
  "."                     { \p _ -> (p, PunctuationT ".") }
  "::"                    { \p _ -> (p, PunctuationT "::") }

  [A-Za-z\-]+             { \p s -> (p, IdentifierT (BSL.toStrict s)) }

  \"([.\n]#[\"])*\"       { \p s -> (p, StringT (BSL.toStrict (BSL.tail (BSL.init s)))) }

{
data Token :: * where
  KeywordT :: ByteString -> Token
  PunctuationT :: ByteString -> Token
  IdentifierT :: ByteString -> Token
  StringT :: ByteString -> Token
  deriving stock (Eq, Show)
}
