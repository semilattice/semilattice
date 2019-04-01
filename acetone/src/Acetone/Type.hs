{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}

module Acetone.Type
  ( Unknown (..)
  , Skolem (..)
  , Type (..)
  ) where

import Data.Word (Word64)

-- |
-- Unknown identifier.
newtype Unknown :: * where
  Unknown :: Word64 -> Unknown
  deriving stock (Eq, Ord, Show)

-- |
-- Skolem identifier.
newtype Skolem :: * where
  Skolem :: Word64 -> Skolem
  deriving stock (Eq, Ord, Show)

-- |
-- Type. Also used for kinds.
data Type :: * where

  -- |
  -- Unknown type (although potentially solved).
  UnknownType :: Unknown -> Type

  -- |
  -- Skolem.
  SkolemType :: Skolem -> Type

  -- |
  -- Type variable.
  VariableType :: Type

  deriving stock (Eq, Show)
