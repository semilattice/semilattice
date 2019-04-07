{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Acetone.Type
  ( -- * Identifiers
    Unknown (..)
  , Skolem (..)

    -- * Types
  , Type (..)

    -- * Universe hiding
  , Type' (..)
  , hideUniverse
  , inUniverse
  ) where

import Acetone.Ast (Name)
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (..))
import Data.Word (Word64)
import GHC.TypeLits (Nat, KnownNat, sameNat)

--------------------------------------------------------------------------------
-- Identifiers

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

--------------------------------------------------------------------------------
-- Types

-- |
-- Type. Also used for kinds, sorts, etc.
data Type :: Nat -> * where

  -- |
  -- Unknown type (although potentially solved).
  UnknownType :: Unknown -> Type 𝔲

  -- |
  -- Skolem.
  SkolemType :: Skolem -> Type 𝔲

  -- |
  -- Global type variable.
  GlobalType :: Name -> Type 𝔲

  -- |
  -- Local type variable.
  LocalType :: Name -> Type 𝔲

  -- |
  -- Type application.
  ApplyType :: Type 𝔲 -> Type 𝔲 -> Type 𝔲

  -- |
  -- Universal quantification.
  ForAllType :: Name -> Type 𝔲 -> Type 𝔲

  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Universe hiding

-- |
-- Like 'Type', but with the universe existentially hidden.
data Type' :: * where
  Type' :: KnownNat 𝔲 => Proxy 𝔲 -> Type 𝔲 -> Type'

hideUniverse :: KnownNat 𝔲 => Type 𝔲 -> Type'
hideUniverse = Type' Proxy

-- |
-- Assert that a type is in a certain universe, at runtime.
inUniverse :: forall 𝔲₁. KnownNat 𝔲₁ => Type' -> Maybe (Type 𝔲₁)
inUniverse (Type' 𝔲₂ τ) =
  case sameNat (Proxy @𝔲₁) 𝔲₂ of
    Nothing -> Nothing
    Just Refl -> Just τ

--------------------------------------------------------------------------------
-- Boring instances

instance Show Type' where
  showsPrec n (Type' _ τ) =
    showParen (n > 10) $
      showString "hideUniverse " .
        showsPrec 11 τ
