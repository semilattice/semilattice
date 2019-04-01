{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}

module Acetone.Ast
  ( -- * Names
    Name (..)

    -- * Items
  , Unit
  , Def (..)
  , TypeExp (..)
  , TermExp (..)
  ) where

import Data.ByteString (ByteString)

--------------------------------------------------------------------------------
-- Names

newtype Name :: * where
  Name :: ByteString -> Name
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Items

-- |
-- Compilation unit.
type Unit =
  [Def]

-- |
-- Global definition.
data Def :: * where
  TypeDef :: Name -> [(Name, TypeExp)] -> TypeExp -> Def
  ValueDef :: Name -> TypeExp -> TermExp -> Def
  deriving stock (Eq, Show)

-- |
-- Type expression. Also used for kinds.
data TypeExp :: * where

  -- |
  -- Type variable.
  VariableTypeExp :: Name -> TypeExp

  deriving stock (Eq, Show)

-- |
-- Term expression.
data TermExp :: * where

  -- |
  -- Evaluate to the value of a variable.
  VariableTermExp :: Name -> TermExp

  -- |
  -- A lambda abstraction eliminates a free variable by creating a function
  -- that has it as its input.
  LambdaTermExp :: Name -> TermExp -> TermExp

  -- |
  -- Apply a function to an argument, returning its result.
  ApplyTermExp :: TermExp -> TermExp -> TermExp

  deriving stock (Eq, Show)
