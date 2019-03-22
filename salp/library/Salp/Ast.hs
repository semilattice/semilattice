{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

module Salp.Ast
  ( -- * Source files
    SourceFile (..)
  , Import (..)

    -- * Definitions
  , TopLevel (..)
  , Method (..)

    -- * Expressions
  , Statement (..)
  , Expression (..)
  , Type (..)

    -- * Miscellaneous
  , Parameter (..)
  , Field (..)
  ) where

import Data.ByteString (ByteString)

--------------------------------------------------------------------------------
-- Source files

data SourceFile =
  SourceFile [Import] [TopLevel]
  deriving stock (Eq, Show)

data Import
  = ClassImport [ByteString] ByteString
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Definitions

data TopLevel
  = Namespace ByteString [TopLevel]
  | Class ByteString [Parameter] [Method]
  | Record ByteString [Field]
  deriving stock (Eq, Show)

data Method =
  Method
    { methodName       :: ByteString
    , methodParameters :: [Parameter]
    , methodReturnType :: Type
    , methodBody       :: [Statement] }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Expressions

data Statement
  = ReturnStatement Expression
  deriving stock (Eq, Show)

data Expression
  = MethodCallExpression Expression ByteString [Expression]
  | SymbolExpression ByteString
  deriving stock (Eq, Show)

data Type
  = StringType
  | SymbolType ByteString
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Miscellaneous

data Parameter =
  Parameter
    { parameterName :: ByteString
    , parameterType :: Type }
  deriving stock (Eq, Show)

data Field =
  Field
    { fieldName :: ByteString
    , fieldType :: Type }
  deriving stock (Eq, Show)
