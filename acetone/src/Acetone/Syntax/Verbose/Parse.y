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
  k_defer                 { ($$, KeywordT "defer") }
  k_division              { ($$, KeywordT "division") }
  k_element               { ($$, KeywordT "element") }
  k_end_evaluate          { ($$, KeywordT "end-evaluate") }
  k_end_record            { ($$, KeywordT "end-record") }
  k_end_record_type       { ($$, KeywordT "end-record-type") }
  k_end_value             { ($$, KeywordT "end-value") }
  k_end_variant_type      { ($$, KeywordT "end-variant-type") }
  k_evaluate              { ($$, KeywordT "evaluate") }
  k_evaluation            { ($$, KeywordT "evaluation") }
  k_external              { ($$, KeywordT "external") }
  k_for                   { ($$, KeywordT "for") }
  k_force                 { ($$, KeywordT "force") }
  k_identification        { ($$, KeywordT "identification") }
  k_interface             { ($$, KeywordT "interface") }
  k_internal              { ($$, KeywordT "internal") }
  k_is                    { ($$, KeywordT "is") }
  k_linkage               { ($$, KeywordT "linkage") }
  k_of                    { ($$, KeywordT "of") }
  k_over                  { ($$, KeywordT "over") }
  k_record                { ($$, KeywordT "record") }
  k_record_type           { ($$, KeywordT "record-type") }
  k_remaining             { ($$, KeywordT "remaining") }
  k_signature             { ($$, KeywordT "signature") }
  k_since                 { ($$, KeywordT "since") }
  k_such                  { ($$, KeywordT "such") }
  k_that                  { ($$, KeywordT "that") }
  k_then                  { ($$, KeywordT "then") }
  k_usage                 { ($$, KeywordT "usage") }
  k_value_id              { ($$, KeywordT "value-id") }
  k_variant_type          { ($$, KeywordT "variant-type") }
  k_when                  { ($$, KeywordT "when") }

  p_colon_colon           { ($$, PunctuationT "::") }
  p_hyphen_greater        { ($$, PunctuationT "->") }
  p_left_parenthesis      { ($$, PunctuationT "(") }
  p_period                { ($$, PunctuationT ".") }
  p_question_question     { ($$, PunctuationT "??") }
  p_right_parenthesis     { ($$, PunctuationT ")") }

  identifier              { (unIdentifier -> Just $$) }

  string                  { (unString -> Just $$) }

%%

Unit
  : { [] }
  | Def Unit
    { $1 <> $2 }

Def
  : k_identification k_division p_period
    k_value_id p_period
      identifier p_period
    k_since p_period
      string p_period
    k_usage p_period
      string p_period

    k_interface k_division p_period
    k_linkage k_is Linkage p_period
    k_signature k_is TypeExp p_period

    k_calculus k_division p_period
      TermExp p_period

    k_end_value p_period

    { -- TODO: Preserve documentation information.
      [ withLocationDef $1 $ ValueSigDef (Name (snd $6)) $21 $25
      , withLocationDef $1 $ ValueDef (Name (snd $6)) $30 ] }

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
  | k_record_type RowTypeBody k_end_record_type
    { withLocationTypeExp $1 $
        ApplyTypeExp (VariableTypeExp (Name "record"))
                     $2 }
  | k_variant_type RowTypeBody k_end_variant_type
    { withLocationTypeExp $1 $
        ApplyTypeExp (VariableTypeExp (Name "variant"))
                     $2 }


TermExp
  : TermExp4 { $1 }

TermExp4
  : TermExp3
    { $1 }
  | TermExp4 TermExp3
    -- TODO: Preserve location information.
    { ApplyTermExp $1 $2 }

TermExp3
  : TermExp2
    { $1 }
  | TermExp3 p_colon_colon identifier
    { withLocationTermExp $2 $
        RecordFieldTermExp $1 (Name (snd $3)) }

TermExp2
  : TermExp1
    { $1 }
  | k_force k_evaluation k_of TermExp1
    { withLocationTermExp $1 $
        ForceTermExp $4 }

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
  | k_defer k_evaluation k_of TermExp
    { withLocationTermExp $1 $
        DeferTermExp $4 }
  | k_record RecordExpBody k_end_record
    { withLocationTermExp $1 $
        RecordTermExp $2 }
  | identifier p_question_question
    { withLocationTermExp (fst $1) $
        LambdaTermExp (Name "?") $
          VariantTermExp (Name (snd $1))
                         (VariableTermExp (Name "?")) }
  | k_evaluate TermExp EvaluateExpBody k_end_evaluate
    { withLocationTermExp $1 $
        EvaluateTermExp $2 $3 }

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

RowTypeBody
  : k_element identifier k_is TypeExp RowTypeBody
    { RowConsTypeExp (Name (snd $2)) $4 $5 }
  | k_remaining k_is TypeExp
    { $3 }
  | { RowNilTypeExp }

RecordExpBody
  : k_element identifier k_is TermExp RecordExpBody
    { (Name (snd $2), $4) : $5 }
  | { [] }

EvaluateExpBody
  : k_when identifier p_question_question identifier k_then TermExp EvaluateExpBody
    { (Name (snd $2), Name (snd $4), $6) : $7 }
  | { [] }

{
unIdentifier :: (AlexPosn, Token) -> Maybe (AlexPosn, ByteString)
unIdentifier (p, IdentifierT n) = Just (p, n)
unIdentifier (_, _) = Nothing

unString :: (AlexPosn, Token) -> Maybe (AlexPosn, ByteString)
unString (p, StringT n) = Just (p, n)
unString (_, _) = Nothing

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
