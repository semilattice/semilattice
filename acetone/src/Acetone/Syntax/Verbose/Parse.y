{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Acetone.Syntax.Verbose.Parse where

import Acetone.Ast

import Acetone.Syntax.Verbose.Lex (AlexPosn (..), Token (..))
import Data.ByteString (ByteString)
}

-- TODO: Return Either instead of crashing.

%name parse
%tokentype { (AlexPosn, Token) }

%token

  k_abstract              { ($$, KeywordT "abstract") }
  k_all                   { ($$, KeywordT "all") }
  k_calculus              { ($$, KeywordT "calculus") }
  k_division              { ($$, KeywordT "division") }
  k_end_value             { ($$, KeywordT "end-value") }
  k_external              { ($$, KeywordT "external") }
  k_for                   { ($$, KeywordT "for") }
  k_identification        { ($$, KeywordT "identification") }
  k_interface             { ($$, KeywordT "interface") }
  k_internal              { ($$, KeywordT "internal") }
  k_is                    { ($$, KeywordT "is") }
  k_linkage               { ($$, KeywordT "linkage") }
  k_over                  { ($$, KeywordT "over") }
  k_signature             { ($$, KeywordT "signature") }
  k_such                  { ($$, KeywordT "such") }
  k_that                  { ($$, KeywordT "that") }
  k_value_id              { ($$, KeywordT "value-id") }

  p_hyphen_greater        { ($$, PunctuationT "->") }
  p_left_parenthesis      { ($$, PunctuationT "(") }
  p_period                { ($$, PunctuationT ".") }
  p_right_parenthesis     { ($$, PunctuationT ")") }

  identifier              { (unIdentifier -> Just $$) }

%%

Unit
  : { [] }
  | Def Unit
    { $1 <> $2 }

Def
  : k_identification k_division p_period
    k_value_id p_period
      identifier p_period

    k_interface k_division p_period
    k_linkage k_is Linkage p_period
    k_signature k_is TypeExp p_period

    k_calculus k_division p_period
      TermExp p_period

    k_end_value p_period

    { [ withLocationDef $1 $ ValueSigDef (Name (snd $6)) $13 $17
      , withLocationDef $1 $ ValueDef (Name (snd $6)) $22 ] }

TypeExp
  : TypeExp3 { $1 }

TypeExp3
  : TypeExp2
    { $1 }
  | TypeExp2 p_hyphen_greater TypeExp3
    { withLocationTypeExp $2 $
        ApplyTypeExp (ApplyTypeExp (VariableTypeExp (Name "function"))
                                   $1)
                     $3 }

TypeExp2
  : TypeExp1
    { $1 }
  | TypeExp2 TypeExp1
    -- TODO: Preserve location information.
    { ApplyTypeExp $1 $2 }

TypeExp1
  : p_left_parenthesis TypeExp p_right_parenthesis
    { $2 }
  | identifier
    { withLocationTypeExp (fst $1) $ VariableTypeExp (Name (snd $1)) }
  | k_for k_all Identifiers1 k_such k_that
      TypeExp
    { withLocationTypeExp $1 $
        let go (_, x) τ = ForAllTypeExp (Name x) τ in
        foldr go $6 $3 }

TermExp
  : TermExp2 { $1 }

TermExp2
  : TermExp1
    { $1 }
  | TermExp2 TermExp1
    -- TODO: Preserve location information.
    { ApplyTermExp $1 $2 }

TermExp1
  : p_left_parenthesis TermExp p_right_parenthesis
    { $2 }
  | identifier
    { withLocationTermExp (fst $1) $ VariableTermExp (Name (snd $1)) }
  | k_over Identifiers1 k_abstract
      TermExp
    { withLocationTermExp $1 $
        let go (_, x) e = LambdaTermExp (Name x) e in
        foldr go $4 $2 }

Linkage
  : k_internal
    { InternalLinkage }
  | k_external
    { ExternalLinkage }

Identifiers0
  : { [] }
  | identifier Identifiers0 { $1 : $2 }

Identifiers1
  : identifier Identifiers0 { $1 : $2 }

{
unIdentifier :: (AlexPosn, Token) -> Maybe (AlexPosn, ByteString)
unIdentifier (p, IdentifierT n) = Just (p, n)
unIdentifier (_, _) = Nothing

mkLocation :: AlexPosn -> Location
mkLocation (AlexPn _ l c) = Location "<unknown>" l c

withLocationDef :: AlexPosn -> Def -> Def
withLocationDef = LocationDef . mkLocation

withLocationTypeExp :: AlexPosn -> TypeExp 𝔲 -> TypeExp 𝔲
withLocationTypeExp = LocationTypeExp . mkLocation

withLocationTermExp :: AlexPosn -> TermExp -> TermExp
withLocationTermExp = LocationTermExp . mkLocation

happyError :: [(AlexPosn, Token)] -> a
happyError = error . show . fmap snd
}
