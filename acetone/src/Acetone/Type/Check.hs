{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Checking a compilation unit is a two-step process:
--
--  1. Extract the signatures.
--  2. Type check all value definitions.
module Acetone.Type.Check
  ( -- * High-level
    checkAll

    -- * Checking
  , Check
  , runCheck
  , withLocation

    -- ** Interfaces
  , extractValueSigs

    -- ** Units
  , checkUnit

    -- ** Type expressions
  , translateTypeExp

    -- ** Term expressions
  , checkTermExp
  , inferTermExp

    -- * Inference
  , Infer
  , runInfer

    -- ** Solving
  , solve
  , unify
  , unify'

    -- ** Environment
  , Γ (..)

    -- ** State
  , Σ (..)
  , freshUnknown
  , freshSkolem
  , instantiate
  , skolemize
  , constrain
  , resolve
  , purge

    -- * Errors
  , Ψ
  , Ψ' (..)
  , throwError

    -- * Optics
  , _γValues
  , _σNext
  , _σConstraints
  , _σResolveds
  ) where

import Acetone.Ast

import Acetone.Type (Skolem (..), Type (..), Type', Unknown (..), hideUniverse, inUniverse)
import Acetone.Type.Constraint (Constraint (..))
import Control.Lens (Lens', (&), (^.), (^?), (?~), (%=), (?=), (<<+=), _2, at, ix, lens, use)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT (..), evalStateT)
import Data.Foldable (for_, traverse_)
import Data.Function (fix)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Word (Word64)
import GHC.TypeLits (KnownNat)

import qualified Control.Monad.Error.Class as Error
import qualified Control.Monad.Reader.Class as Reader
import qualified Data.Map as Map
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- High-level

checkAll :: Unit -> Either Ψ ()
checkAll ds = runCheck $
  let xs = extractValueSigs ds in
  checkUnit (fmap (^. _2) xs) ds

--------------------------------------------------------------------------------
-- Check

type Check =
  ReaderT Location (Either Ψ)

runCheck :: Check a -> Either Ψ a
runCheck a = runReaderT a (Location "" 0 0)

withLocation :: (MonadReader Location f) => Location -> f a -> f a
withLocation = Reader.local . const

--------------------------------------------------------------------------------
-- Extract

extractValueSigs :: Unit -> Map Name (Linkage, Type 0)
extractValueSigs =
  -- TODO: Handle duplicate definitions.
  foldMap $ fix $ \f -> \case
    LocationDef _ d   -> (f d)
    ValueSigDef n l τ -> Map.singleton n (l, translateTypeExp Set.empty τ)
    ValueDef{}        -> Map.empty

--------------------------------------------------------------------------------
-- Units

-- |
-- Given the interface, type check a unit.
checkUnit :: Map Name (Type 0) -> Unit -> Check ()
checkUnit xs ds =
  for_ ds $ fix $ \f -> \case
    LocationDef l d -> withLocation l (f d)
    ValueSigDef{}   -> pure ()
    ValueDef x e    ->
      case xs ^? ix x of
        Nothing -> throwError $ UnknownValue x
        Just τ  -> checkTermExp xs τ e

--------------------------------------------------------------------------------
-- Type expressions

-- |
-- Given a type expression, return the corresponding type.
translateTypeExp :: Set Name -> TypeExp 𝔲 -> Type 𝔲

translateTypeExp γ (LocationTypeExp _ τ) =
  translateTypeExp γ τ

translateTypeExp γ (VariableTypeExp x)
  | x `Set.member` γ = LocalType x
  | otherwise        = GlobalType x

translateTypeExp γ (ApplyTypeExp τ₁ τ₂) =
  ApplyType (translateTypeExp γ τ₁)
            (translateTypeExp γ τ₂)

translateTypeExp γ (ForAllTypeExp x τ) =
  ForAllType x (translateTypeExp (Set.insert x γ) τ)

--------------------------------------------------------------------------------
-- Term expressions

-- |
-- Check that a term has the given expected type. The given type will first be
-- Skolemized.
checkTermExp :: Map Name (Type 0) -> Type 0 -> TermExp -> Check ()
checkTermExp xs τ e =
  runInfer $ do

    -- Check that term has expected type.
    let γ = Γ { γValues = xs }
    τ' <- skolemize τ
    τe <- inferTermExp γ e
    constrain $ τ' :~: τe

    -- Solve constraints.
    use _σConstraints >>= traverse_ solve

inferTermExp :: Γ -> TermExp -> Infer (Type 0)

inferTermExp γ (LocationTermExp l e) =
  withLocation l (inferTermExp γ e)

inferTermExp γ (VariableTermExp x) =
  case γ ^? _γValues . ix x of
    Nothing -> throwError (UnknownValue x)
    Just τ -> instantiate τ

inferTermExp γ (LambdaTermExp x e) = do
  τx <- UnknownType <$> freshUnknown

  let γe = γ & _γValues . at x ?~ τx
  τe <- inferTermExp γe e

  -- TODO: Replace (Name "function") type by intrinsic.
  let τλ = ApplyType (ApplyType (GlobalType (Name "function")) τx) τe

  pure τλ

inferTermExp γ (ApplyTermExp e₁ e₂) = do
  τe₁ <- inferTermExp γ e₁
  τe₂ <- inferTermExp γ e₂

  τr <- UnknownType <$> freshUnknown
  -- TODO: Replace (Name "function") type by intrinsic.
  constrain $ τe₁ :~: ApplyType (ApplyType (GlobalType (Name "function")) τe₂) τr

  pure τr

--------------------------------------------------------------------------------
-- Inference

type Infer =
  StateT Σ Check

runInfer :: Infer a -> Check a
runInfer a =
  let σ = Σ { σNext = 0, σConstraints = [], σResolveds = Map.empty } in
  evalStateT a σ

--------------------------------------------------------------------------------
-- Solving

solve :: Constraint -> Infer ()
solve (τ₁ :~: τ₂) = unify τ₁ τ₂

unify :: KnownNat 𝔲 => Type 𝔲 -> Type 𝔲 -> Infer ()
unify = \τ₁ τ₂ -> do
  τ₁' <- purge τ₁
  τ₂' <- purge τ₂
  unify' τ₁' τ₂'

unify' :: KnownNat 𝔲 => Type 𝔲 -> Type 𝔲 -> Infer ()

unify' (UnknownType υ₁) (UnknownType υ₂) | υ₁ == υ₂ = pure ()
unify' (UnknownType υ) τ = resolve υ τ
unify' τ (UnknownType υ) = resolve υ τ

unify' (SkolemType s₁) (SkolemType s₂) | s₁ == s₂ = pure ()
unify' τ₁@SkolemType{} τ₂ = cannotUnify τ₁ τ₂
unify' τ₂ τ₁@SkolemType{} = cannotUnify τ₁ τ₂

unify' (GlobalType x₁) (GlobalType x₂) | x₁ == x₂ = pure ()
unify' τ₁@GlobalType{} τ₂ = cannotUnify τ₁ τ₂
unify' τ₂ τ₁@GlobalType{} = cannotUnify τ₁ τ₂

unify' (LocalType x₁) (LocalType x₂) | x₁ == x₂ = pure ()
unify' τ₁@LocalType{} τ₂ = cannotUnify τ₁ τ₂
unify' τ₂ τ₁@LocalType{} = cannotUnify τ₁ τ₂

unify' (ApplyType τ₁ τ₂) (ApplyType τ₃ τ₄) = do { unify τ₁ τ₃; unify τ₂ τ₄ }

unify' ForAllType{} _ = throwError HigherRankType
unify' _ ForAllType{} = throwError HigherRankType

cannotUnify :: KnownNat 𝔲 => Type 𝔲 -> Type 𝔲 -> Infer a
cannotUnify τ₁ τ₂ = do
  τ₁' <- purge' τ₁
  τ₂' <- purge' τ₂
  throwError $ CannotUnify τ₁' τ₂'

--------------------------------------------------------------------------------
-- Environment

newtype Γ :: * where
  Γ
    :: { γValues :: Map Name (Type 0) }
    -> Γ

--------------------------------------------------------------------------------
-- State

data Σ :: * where
  Σ
    :: { σNext        :: Word64
       , σConstraints :: [Constraint]
       , σResolveds   :: Map Unknown Type' }
    -> Σ

freshUnknown :: Infer Unknown
freshUnknown = fmap Unknown $ _σNext <<+= 1

freshSkolem :: Infer Skolem
freshSkolem = fmap Skolem $ _σNext <<+= 1

instantiate :: Type 𝔲 -> Infer (Type 𝔲)
instantiate = instantemize (UnknownType <$> freshUnknown)

skolemize :: Type 𝔲 -> Infer (Type 𝔲)
skolemize = instantemize (SkolemType <$> freshSkolem)

-- |
-- Shared implementation of 'instantiate' and 'skolemize'.
instantemize :: Infer (Type 𝔲) -> Type 𝔲 -> Infer (Type 𝔲)
instantemize φ = go0 Map.empty
  where
  go0 γ (ForAllType x τ) = do { x' <- φ; go0 (Map.insert x x' γ) τ }
  go0 γ τ = go1 γ τ

  go1 _ τ@UnknownType{}   = pure τ
  go1 _ τ@SkolemType{}    = pure τ
  go1 _ τ@GlobalType{}    = pure τ
  go1 γ τ@(LocalType x)   = pure $ fromMaybe τ (γ ^? ix x)
  go1 γ (ApplyType τ₁ τ₂) = ApplyType <$> go1 γ τ₁ <*> go1 γ τ₂
  go1 _ ForAllType{}      = throwError HigherRankType

constrain :: Constraint -> Infer ()
constrain = (_σConstraints %=) . (:)

-- |
-- Resolve an unknown.
resolve :: KnownNat 𝔲 => Unknown -> Type 𝔲 -> Infer ()
resolve υ τ = _σResolveds . at υ ?= hideUniverse τ

-- |
-- Follow resolveds recursively until an unresolved unknown or a known type is
-- found. Will fail if the universe of the result is different from the
-- universe of the input, but this shouldn't happen.
purge :: forall 𝔲. KnownNat 𝔲 => Type 𝔲 -> Infer (Type 𝔲)
purge τ@(UnknownType υ) =
  use (_σResolveds . at υ) >>= do
    maybe (pure τ) $
      maybe (throwError PurgeInvalidUniverse) purge .
        inUniverse @𝔲
purge τ = pure τ

-- |
-- Like 'purge', but purge the entire tree instead of just the root.
purge' :: KnownNat 𝔲 => Type 𝔲 -> Infer (Type 𝔲)
purge' τ =
  purge τ >>= \case
    τ'@UnknownType{}  -> pure τ'
    τ'@SkolemType{}   -> pure τ'
    τ'@GlobalType{}   -> pure τ'
    τ'@LocalType{}    -> pure τ'
    ApplyType τ'₁ τ'₂ -> ApplyType <$> purge' τ'₁ <*> purge' τ'₂
    ForAllType{}      -> throwError HigherRankType

--------------------------------------------------------------------------------
-- Errors

type Ψ = (Location, Ψ')

data Ψ' :: * where
  UnknownValue :: Name -> Ψ'
  CannotUnify :: Type 𝔲 -> Type 𝔲 -> Ψ'
  PurgeInvalidUniverse :: Ψ'
  HigherRankType :: Ψ'
deriving stock instance Show Ψ'

throwError :: (MonadReader Location f, MonadError Ψ f) => Ψ' -> f a
throwError ψ = do
  l <- Reader.ask
  Error.throwError (l, ψ)

--------------------------------------------------------------------------------
-- Optics

_γValues :: Lens' Γ (Map Name (Type 0))
_γValues = lens γValues (\s a -> s { γValues = a })

_σNext :: Lens' Σ Word64
_σNext = lens σNext (\s a -> s { σNext = a })

_σConstraints :: Lens' Σ [Constraint]
_σConstraints = lens σConstraints (\s a -> s { σConstraints = a })

_σResolveds :: Lens' Σ (Map Unknown Type')
_σResolveds = lens σResolveds (\s a -> s { σResolveds = a })
