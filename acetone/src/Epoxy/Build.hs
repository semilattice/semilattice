{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}

-- |
-- EDSL for building ANF let programs.
module Epoxy.Build
  ( -- * Infrastructure
    B
  , Ξ
  , runB
  , runΞ

    -- * Programs
  , let_
  , letRec

    -- * Expressions
  , lambda
  , case_
  , reduction

    -- * Values
  , local
  , global
  , constant
  ) where

import Epoxy.Anf

import Control.Lens (Lens', (&), (<<+=), lens)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (ContT (..), runContT)
import Control.Monad.Trans.State (State, evalState)
import Data.Map (Map)
import Data.Traversable (for)
import Data.Word (Word64)

--------------------------------------------------------------------------------
-- Infrastructure

type B = State Σ
type Ξ = ContT (Let Val) B

data Σ :: * where
  Σ :: { σNext :: Word64 } -> Σ

runB :: B (Let Val) -> Let Val
runB b = evalState b σ₀

runΞ :: Ξ Val -> B (Let Val)
runΞ ξ = runContT ξ (pure . Return)

σ₀ :: Σ
σ₀ = Σ 0

fresh :: MonadState Σ m => m Local
fresh = _σNext <<+= 1 & fmap Local

--------------------------------------------------------------------------------
-- Programs

let_ :: Exp Val -> Ξ Val
let_ e = ContT $ \ξ -> do
  x <- fresh
  Let x e <$> ξ (LocalVal x)

letRec :: () -- TODO: Generate LetRec.
letRec = undefined

--------------------------------------------------------------------------------
-- Expressions

lambda :: (Val -> Ξ Val) -> Ξ Val
lambda λ = do
  x <- fresh
  e <- lift $ runΞ (λ (LocalVal x))
  let_ (LambdaExp x e)

case_ :: Val -> Map Discriminator (Val -> Ξ Val) -> Ξ Val
case_ e ξs = do
  ξs' <- for ξs $ \ξ -> do
    x <- fresh
    r <- lift $ runΞ (ξ (LocalVal x))
    pure (x, r)
  let_ (CaseExp e ξs')

reduction :: Red Val -> Ξ Val
reduction r = let_ (ReductionExp r)

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
