{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}

module Acetone.Ast
  ( -- * Names
    Name (..)
  , Linkage (..)

    -- * Locations
  , Location (..)

    -- * Items
  , Unit
  , Def (..)
  , TypeExp (..)
  , TermExp (..)
  ) where

import Data.ByteString (ByteString)
import GHC.TypeLits (Nat)

--------------------------------------------------------------------------------
-- Names

newtype Name :: * where
  Name :: ByteString -> Name
  deriving stock (Eq, Ord, Show)

data Linkage :: * where
  InternalLinkage :: Linkage
  ExternalLinkage :: Linkage
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Locations

data Location =
  Location
    { locationFile   :: FilePath
    , locationLine   :: {-# UNPACK #-} !Int
    , locationColumn :: {-# UNPACK #-} !Int }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Items

-- |
-- Compilation unit. May be drawn from more than one source file. Contains all
-- the definitions that are compiled together.
type Unit =
  [Def]

-- |
-- Global definition.
data Def :: * where

  -- |
  -- Add location information to a definition.
  LocationDef :: Location -> Def -> Def

  -- |
  -- Declaration of a value.
  ValueSigDef :: Name -> Linkage -> TypeExp 0 -> Def

  -- |
  -- Definition of a value.
  ValueDef :: Name -> TermExp -> Def

  deriving stock (Eq, Show)

-- |
-- Type expression. Also used for kinds, sorts, etc.
data TypeExp :: Nat -> * where

  -- |
  -- Add location information to a type expression.
  LocationTypeExp :: Location -> TypeExp 𝔲 -> TypeExp 𝔲

  -- |
  -- Type variable.
  VariableTypeExp :: Name -> TypeExp 𝔲

  -- |
  -- Type application.
  ApplyTypeExp :: TypeExp 𝔲 -> TypeExp 𝔲 -> TypeExp 𝔲

  deriving stock (Eq, Show)

-- |
-- Term expression.
data TermExp :: * where

  -- |
  -- Add location information to a term expression.
  LocationTermExp :: Location -> TermExp -> TermExp

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
