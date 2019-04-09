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
import Data.Bifunctor (first, second)
import Data.Foldable (for_, traverse_)
import Data.Function (fix)
import Data.List (sortBy)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Traversable (for)
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

translateTypeExp γ (RowConsTypeExp x τ₁ τ₂) =
  RowConsType x (translateTypeExp γ τ₁)
              (translateTypeExp γ τ₂)

translateTypeExp _ RowNilTypeExp =
  RowNilType

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

inferTermExp γ (DeferTermExp e) = do
  τe <- inferTermExp γ e
  -- TODO: Replace (Name "lazy") type by intrinsic.
  pure $ ApplyType (GlobalType (Name "lazy"))
                  τe

inferTermExp γ (ForceTermExp e) = do
  τe <- inferTermExp γ e
  τr <- UnknownType <$> freshUnknown
  -- TODO: Replace (Name "lazy") type by intrinsic.
  constrain $ τe :~: ApplyType (GlobalType (Name "lazy")) τr
  pure τr

inferTermExp γ (RecordTermExp es) = do
  -- TODO: Check for duplicate elements.
  τes <- traverse (traverse (inferTermExp γ)) es
  -- TODO: Replace (Name "record") type by intrinsic.
  pure $ ApplyType (GlobalType (Name "record"))
                   (foldr (uncurry RowConsType) RowNilType τes)

inferTermExp γ (RecordFieldTermExp e x) = do
  τe <- inferTermExp γ e

  τx <- UnknownType <$> freshUnknown
  τρ <- UnknownType <$> freshUnknown
  -- TODO: Replace (Name "record") type by intrinsic.
  constrain $ τe :~: ApplyType (GlobalType (Name "record"))
                               (RowConsType x τx τρ)

  pure τx

inferTermExp γ (VariantTermExp x e) = do
  τe <- inferTermExp γ e
  τρ <- UnknownType <$> freshUnknown
  -- TODO: Replace (Name "variant") type by intrinsic.
  pure $ ApplyType (GlobalType (Name "variant"))
                   (RowConsType x τe τρ)

inferTermExp γ (EvaluateTermExp e₁ es) = do
  τe₁ <- inferTermExp γ e₁

  τr <- UnknownType <$> freshUnknown
  τes <- for es $ \(x, x', e₂) -> do
    τx' <- UnknownType <$> freshUnknown
    let γe₂ = γ & _γValues . at x' ?~ τx'
    τe₂ <- inferTermExp γe₂ e₂
    constrain $ τe₂ :~: τr
    pure (x, τx')

  -- TODO: Replace (Name "variant") type by intrinsic.
  constrain $ τe₁ :~: ApplyType (GlobalType (Name "variant"))
                                (foldr (uncurry RowConsType) RowNilType τes)

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

unify' τ₁@RowConsType{} τ₂ = unifyRows τ₁ τ₂
unify' τ₁ τ₂@RowConsType{} = unifyRows τ₁ τ₂

unify' τ₁@RowNilType{} τ₂ = unifyRows τ₁ τ₂
unify' τ₁ τ₂@RowNilType{} = unifyRows τ₁ τ₂

cannotUnify :: KnownNat 𝔲 => Type 𝔲 -> Type 𝔲 -> Infer a
cannotUnify τ₁ τ₂ = do
  τ₁' <- purge' τ₁
  τ₂' <- purge' τ₂
  throwError $ CannotUnify τ₁' τ₂'

--------------------------------------------------------------------------------
-- Row type unification

-- DISCLAIMER: This code was copied from the PureScript compiler and adapted to
-- DISCLAIMER: work with the Acetone AST and Type types. Credit goes to the
-- DISCLAIMER: original authors. Naming and formatting are largely preserved
-- DISCLAIMER: and hence inconsistent with the rest of this code base.

data RowListItem 𝔲 = RowListItem
  { rowListLabel :: Name
  , _rowListType :: Type 𝔲 }

unifyRows :: forall 𝔲. KnownNat 𝔲 => Type 𝔲 -> Type 𝔲 -> Infer ()
unifyRows r1 r2 = sequence_ matches *> uncurry unifyTails rest where
  (matches, rest) = alignRowsWith unify r1 r2

  unifyTails :: ([RowListItem 𝔲], Type 𝔲) -> ([RowListItem 𝔲], Type 𝔲) -> Infer ()

  unifyTails ([], UnknownType u) (sd, r) = resolve u (rowFromList (sd, r))
  unifyTails (sd, r) ([], UnknownType u) = resolve u (rowFromList (sd, r))
  unifyTails (sd1, UnknownType u1) (sd2, UnknownType u2) = do
    -- TODO: Occurs check.
    rest' <- UnknownType <$> freshUnknown
    resolve u1 (rowFromList (sd2, rest'))
    resolve u2 (rowFromList (sd1, rest'))

  unifyTails ([], SkolemType s1) ([], SkolemType s2) | s1 == s2 = return ()
  unifyTails (_, SkolemType{}) _ = cannotUnify r1 r2
  unifyTails _ (_, SkolemType{}) = cannotUnify r1 r2

  unifyTails ([], GlobalType v1) ([], GlobalType v2) | v1 == v2 = return ()
  unifyTails (_, GlobalType{}) _ = cannotUnify r1 r2
  unifyTails _ (_, GlobalType{}) = cannotUnify r1 r2

  unifyTails ([], LocalType v1) ([], LocalType v2) | v1 == v2 = return ()
  unifyTails (_, LocalType{}) _ = cannotUnify r1 r2
  unifyTails _ (_, LocalType{}) = cannotUnify r1 r2

  unifyTails (_, ApplyType{}) _ = cannotUnify r1 r2
  unifyTails _ (_, ApplyType{}) = cannotUnify r1 r2

  unifyTails (_, ForAllType{}) _ = throwError HigherRankType
  unifyTails _ (_, ForAllType{}) = throwError HigherRankType

  unifyTails (_, RowConsType{}) _ = cannotUnify r1 r2 -- Should never happen?
  unifyTails _ (_, RowConsType{}) = cannotUnify r1 r2 -- Should never happen?

  unifyTails ([], RowNilType) ([], RowNilType) = return ()
  unifyTails (_, RowNilType) _ = return ()
  unifyTails _ (_, RowNilType) = return ()

alignRowsWith
  :: (Type 𝔲 -> Type 𝔲 -> r)
  -> Type 𝔲
  -> Type 𝔲
  -> ([r], (([RowListItem 𝔲], Type 𝔲), ([RowListItem 𝔲], Type 𝔲)))
alignRowsWith f ty1 ty2 = go s1 s2 where
  (s1, tail1) = rowToSortedList ty1
  (s2, tail2) = rowToSortedList ty2

  go [] r = ([], (([], tail1), (r, tail2)))
  go r [] = ([], ((r, tail1), ([], tail2)))
  go lhs@(RowListItem l1 t1 : r1) rhs@(RowListItem l2 t2 : r2)
    | l1 < l2 = (second . first . first) (RowListItem l1 t1 :) (go r1 rhs)
    | l2 < l1 = (second . second . first) (RowListItem l2 t2 :) (go lhs r2)
    | otherwise = first (f t1 t2 :) (go r1 r2)

rowToList :: Type 𝔲 -> ([RowListItem 𝔲], Type 𝔲)
rowToList = go where
  go (RowConsType name ty row) =
    first (RowListItem name ty :) (rowToList row)
  go r = ([], r)

rowToSortedList :: Type 𝔲 -> ([RowListItem 𝔲], Type 𝔲)
rowToSortedList = first (sortBy (comparing rowListLabel)) . rowToList

rowFromList :: ([RowListItem 𝔲], Type 𝔲) -> Type 𝔲
rowFromList (xs, r) = foldr (\(RowListItem name ty) -> RowConsType name ty) r xs

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

  go1 _ τ@UnknownType{}       = pure τ
  go1 _ τ@SkolemType{}        = pure τ
  go1 _ τ@GlobalType{}        = pure τ
  go1 γ τ@(LocalType x)       = pure $ fromMaybe τ (γ ^? ix x)
  go1 γ (ApplyType τ₁ τ₂)     = ApplyType <$> go1 γ τ₁ <*> go1 γ τ₂
  go1 _ ForAllType{}          = throwError HigherRankType
  go1 γ (RowConsType x τ₁ τ₂) = RowConsType x <$> go1 γ τ₁ <*> go1 γ τ₂
  go1 _ τ@RowNilType          = pure τ

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
    τ'@UnknownType{}      -> pure τ'
    τ'@SkolemType{}       -> pure τ'
    τ'@GlobalType{}       -> pure τ'
    τ'@LocalType{}        -> pure τ'
    ApplyType τ'₁ τ'₂     -> ApplyType <$> purge' τ'₁ <*> purge' τ'₂
    ForAllType{}          -> throwError HigherRankType
    RowConsType x τ'₁ τ'₂ -> RowConsType x <$> purge' τ'₁ <*> purge' τ'₂
    τ'@RowNilType         -> pure τ'

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
