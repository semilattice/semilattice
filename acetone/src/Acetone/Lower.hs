-- |
-- Conversion from AST into ANF.
module Acetone.Lower
  ( -- * Units
    lowerUnit

    -- * Definitions
  , lowerDef

    -- * Term expressions
  , Γ
  , lowerTermExp
  ) where

import Control.Lens ((&), (^?), (?~), at, ix)
import Data.Map (Map)
import Data.Traversable (for)
import Epoxy.Build (B)

import qualified Acetone.Ast as A
import qualified Data.Map as Map
import qualified Epoxy.Anf as E
import qualified Epoxy.Build as B

--------------------------------------------------------------------------------
-- Units

lowerUnit :: A.Unit -> E.Unit E.Val
lowerUnit = foldMap lowerDef

--------------------------------------------------------------------------------
-- Definitions

lowerDef :: A.Def -> E.Unit E.Val

lowerDef (A.LocationDef _ d) =
  lowerDef d

lowerDef A.ValueSigDef{} =
  Map.empty

lowerDef (A.ValueDef (A.Name x) e) =
  Map.singleton (E.Global x)
                (B.runB (lowerTermExp Map.empty e))

--------------------------------------------------------------------------------
-- Term expressions

type Γ = Map A.Name E.Val

lowerTermExp :: Γ -> A.TermExp -> B E.Val

lowerTermExp γ (A.LocationTermExp _ e) =
  lowerTermExp γ e

lowerTermExp γ (A.VariableTermExp x) =
  case γ ^? ix x of
    Nothing -> B.global $ E.Global (A.unName x)
    Just v  -> pure v

lowerTermExp γ (A.LambdaTermExp x e) =
  B.lambda $ \x' ->
    let γ' = γ & at x ?~ x' in
    lowerTermExp γ' e

lowerTermExp γ (A.ApplyTermExp e₁ e₂) = do
  e₁' <- lowerTermExp γ e₁
  e₂' <- lowerTermExp γ e₂
  B.reduction $ E.ApplyRed e₁' e₂'

lowerTermExp γ (A.DeferTermExp e) = do
  e' <- B.lambda $ \_ -> lowerTermExp γ e
  B.reduction $ E.LazyRed e'

lowerTermExp γ (A.ForceTermExp e) = do
  e' <- lowerTermExp γ e
  u  <- B.reduction $ E.RecordRed Map.empty
  B.reduction $ E.ApplyRed e' u

lowerTermExp γ (A.RecordTermExp es) = do
  es' <- for es $ \(x, e) -> do
           e' <- lowerTermExp γ e
           pure (E.Field (A.unName x), e')
  B.reduction $ E.RecordRed (Map.fromList es')

lowerTermExp γ (A.RecordFieldTermExp e x) = do
  e' <- lowerTermExp γ e
  B.reduction $ E.ProjectRed e' (E.Field (A.unName x))

lowerTermExp γ (A.VariantTermExp x e) = do
  e' <- lowerTermExp γ e
  B.reduction $ E.InjectRed (E.Discriminator (A.unName x)) e'

lowerTermExp γ (A.EvaluateTermExp e₁ e₂s) = do
  e₁' <- lowerTermExp γ e₁
  let e₂s' = Map.fromList [ (E.Discriminator (A.unName x), e₂')
                          | (x, x', e₂) <- e₂s
                          , let e₂' = \x'' ->
                                  let γ' = γ & at x' ?~ x'' in
                                  lowerTermExp γ' e₂ ]
  B.case_ e₁' e₂s'
