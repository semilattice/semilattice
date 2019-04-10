{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLists #-}

module Epoxy.Anf
  ( -- * Units
    Unit

    -- * Programs
  , Let (..)
  , Exp (..)
  , Red (..)
  , Val (..)

    -- * Names
  , Local (..)
  , Global (..)
  , Field (..)
  , Discriminator (..)

    -- * Constants
  , Constant (..)

    -- * Free locals
  , HasFree (..)
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Set (Set, (\\))
import Data.Word (Word64)

import qualified Data.Map.Strict as M

--------------------------------------------------------------------------------
-- Units

type Unit v =
  Map Global (Let v)

--------------------------------------------------------------------------------
-- Programs

-- |
-- Let bindings.
data Let :: * -> * where

  -- |
  -- A strict let binding and a continuation.
  Let :: Local -> Exp v -> Let v -> Let v

  -- |
  -- A collection of mutually-recursive lazy let bindings and a continuation.
  -- The bindings are unordered: the order in which they are evaluated is not
  -- important, because they are lazy. The bindings are unique: no two bindings
  -- bind the same variable, significantly simplifying transformation
  -- algorithms.
  LetRec :: Map Local (Exp v) -> Let v -> Let v

  -- |
  -- Just return a value; by construction the final continuation.
  Return :: v -> Let v

  deriving stock (Eq, Show)
  deriving stock (Foldable, Functor, Traversable)

-- |
-- Expression that computes something given values.
data Exp :: * -> * where

  -- |
  -- A lambda abstraction eliminates a free variable by creating a function
  -- that has it as its input.
  LambdaExp :: Local -> Let v -> Exp v

  -- |
  -- Analyze a variant by giving an expression for each possible discriminator.
  CaseExp :: v -> Map Discriminator (Local, Let v) -> Exp v

  -- |
  -- See 'Red'.
  ReductionExp :: Red v -> Exp v

  deriving stock (Eq, Show)
  deriving stock (Foldable, Functor, Traversable)

-- |
-- Expression that does not bind variables; its evaluation is entirely
-- bottom-up.
data Red :: * -> * where

  -- |
  -- Apply a function to an argument, returning its result.
  ApplyRed :: v -> v -> Red v

  -- |
  -- Construct a record from fields and their values.
  RecordRed :: Map Field v -> Red v

  -- |
  -- Project a record field by its name.
  ProjectRed :: v -> Field -> Red v

  -- |
  -- Construct a variant from a discriminator and a value.
  InjectRed :: Discriminator -> v -> Red v

  -- |
  -- Given a lambda that ignores its argument, memoize it. This can be used to
  -- implement lazy evaluation.
  LazyRed :: v -> Red v

  deriving stock (Eq, Show)
  deriving stock (Foldable, Functor, Traversable)

-- |
-- Values are atomic: they do not consist of other values.
data Val :: * where

  -- |
  -- Evaluate to the value of a local variable.
  LocalVal :: Local -> Val

  -- |
  -- Evaluate to the value of a top level definition.
  GlobalVal :: Global -> Val

  -- |
  -- Evaluate to a constant.
  ConstantVal :: Constant -> Val

  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Names

-- |
-- Name of a local variable; one that was introduced by a let binding or by a
-- lambda abstraction.
newtype Local :: * where
  Local :: Word64 -> Local
  deriving stock (Eq, Ord, Show)

-- |
-- Name of a global variable; one that was defined at the top level.
newtype Global :: * where
  Global :: ByteString -> Global
  deriving stock (Eq, Ord, Show)

-- |
-- Field of a record.
newtype Field :: * where
  Field :: ByteString -> Field
  deriving stock (Eq, Ord, Show)

-- |
-- Discriminator of a variant.
newtype Discriminator :: * where
  Discriminator :: ByteString -> Discriminator
  deriving stock (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Constants

-- |
-- Constant value; does not have free variables and can be quoted into the
-- compiled program.
data Constant :: * where
  BoolConstant :: Bool -> Constant
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Free locals

-- |
-- Find the locals that an expression expects to be in scope when being
-- evaluated.
class HasFree a where
  free :: a -> Set Local

instance HasFree v => HasFree (Let v) where
  free (Let x e₁ e₂) = free e₁ <> (free e₂ \\ [x])
  free (LetRec bs e) = (foldMap free bs <> free e) \\ M.keysSet bs
  free (Return e) = free e

instance HasFree v => HasFree (Exp v) where
  free (LambdaExp x e)  = free e \\ [x]
  free (CaseExp e₁ cs)  = free e₁ <> foldMap (\(x, e₂) -> free e₂ \\ [x]) cs
  free (ReductionExp r) = free r

instance HasFree v => HasFree (Red v) where
  free = foldMap free

instance HasFree Val where
  free (LocalVal x)  = [x]
  free GlobalVal{}   = []
  free ConstantVal{} = []
