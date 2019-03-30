{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLists #-}

module Acetone.Anf
  ( -- * Programs
    Let (..)
  , Exp (..)
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
-- Programs

-- |
-- Set of recursive bindings together with a result. Each binding is in scope
-- of the result and in all the other bindings. The bindings are unordered: the
-- order in which they are evaluated is not important, because they are devoid
-- of side effects. The bindings are unique: no two bindings bind the same
-- variable, significantly simplifying transformation algorithms.
data Let :: * -> * where
  Let :: Map Local (Exp v) -> v -> Let v
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
  -- Apply a function to an argument, returning its result.
  ApplyExp :: v -> v -> Exp v

  -- |
  -- Construct a record from fields and their values.
  RecordExp :: Map Field v -> Exp v

  -- |
  -- Project a record field by its name.
  ProjectExp :: v -> Field -> Exp v

  -- |
  -- Construct a variant from a discriminator and a value.
  InjectExp :: Discriminator -> v -> Exp v

  -- |
  -- Analyze a variant by giving an expression for each possible discriminator.
  CaseExp :: v -> Map Discriminator (Local, Let v) -> Exp v

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
  Global :: Word64 -> Global
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
  free (Let bs r) = (foldMap free bs <> free r) \\ M.keysSet bs

instance HasFree v => HasFree (Exp v) where
  free (LambdaExp x e)  = free e \\ [x]
  free (ApplyExp e₁ e₂) = free e₁ <> free e₂
  free (RecordExp fs)   = foldMap free fs
  free (ProjectExp e _) = free e
  free (InjectExp _ e)  = free e
  free (CaseExp e₁ cs)  = free e₁ <> foldMap (\(x, e₂) -> free e₂ \\ [x]) cs

instance HasFree Val where
  free (LocalVal x)  = [x]
  free GlobalVal{}   = []
  free ConstantVal{} = []
