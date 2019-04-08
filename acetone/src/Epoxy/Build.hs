{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- EDSL for building ANF let programs.
module Epoxy.Build
  ( -- * Infrastructure
    B
  , runB

    -- * Expressions
  , lambda
  , apply
  , record
  , project
  , inject
  , case_

    -- * Values
  , local
  , global
  , constant
  ) where

import Epoxy.Anf

import Control.Lens (Lens', (&), (?=), (<<.=), (<<+=), at, lens)
import Control.Monad.Trans.State (State, runState)
import Data.Map (Map)
import Data.Word (Word64)

import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- Infrastructure

newtype B a =
  B { unB :: State Σ a }
  deriving newtype (Applicative, Functor, Monad)

data Σ =
  Σ
    { σNext     :: Word64
    , σBindings :: Map Local (Exp Val) }

runB :: B Val -> Let Val
runB (B b) =
  let σ = Σ 0 Map.empty in
  let (r, σ') = runState b σ in
  Let (σBindings σ') r

bind :: Exp Val -> B Val
bind e = B $ do
  x <- _σNext <<+= 1 & fmap Local
  _σBindings . at x ?= e
  pure (LocalVal x)

lift :: (Val -> B Val) -> B (Local, Let Val)
lift λ = B $ do
  x <- _σNext <<+= 1 & fmap Local

  oldBindings <- _σBindings <<.= Map.empty
  r <- unB (λ (LocalVal x))
  bs <- _σBindings <<.= oldBindings

  pure (x, Let bs r)

--------------------------------------------------------------------------------
-- Expressions

lambda :: (Val -> B Val) -> B Val
lambda λ = do { (x, e) <- lift λ; bind (LambdaExp x e) }

apply :: Val -> Val -> B Val
apply e₁ e₂ = bind (ApplyExp e₁ e₂)

record :: Map Field Val -> B Val
record es = bind (RecordExp es)

project :: Val -> Field -> B Val
project e x = bind (ProjectExp e x)

inject :: Discriminator -> Val -> B Val
inject x e = bind (InjectExp x e)

case_ :: Val -> Map Discriminator (Val -> B Val) -> B Val
case_ e λs = do { λs' <- traverse lift λs; bind (CaseExp e λs') }

--------------------------------------------------------------------------------
-- Values

local :: Applicative f => Local -> f Val
local = pure . LocalVal

global :: Applicative f => Global -> f Val
global = pure . GlobalVal

constant :: Applicative f => Constant -> f Val
constant = pure . ConstantVal

--------------------------------------------------------------------------------
-- Optics

_σNext :: Lens' Σ Word64
_σNext = lens σNext (\s a -> s { σNext = a })

_σBindings :: Lens' Σ (Map Local (Exp Val))
_σBindings = lens σBindings (\s a -> s { σBindings = a })
