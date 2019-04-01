{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Acetone.Type.Constraint
  ( Constraint (..)
  ) where

import Acetone.Type (Type)
import GHC.TypeLits (KnownNat)

data Constraint :: * where
  (:~:) :: KnownNat 𝔲 => Type 𝔲 -> Type 𝔲 -> Constraint
