{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}

module Acetone.Interface
  ( Interface (..)
  , empty
  ) where

import Acetone.Ast (Name)
import Acetone.Type (Type)
import Data.Map (Map)

import qualified Data.Map as Map

-- |
-- The interface of a compilation unit contains all the information necessary
-- to type check a unit. It contains information about all global definitions.
data Interface :: * where
  Interface
    :: { interfaceTypes  :: Map Name ([(Name, Type 1)], Type 1, Type 0)
       , interfaceValues :: Map Name (Type 0) }
    -> Interface
  deriving stock (Eq, Show)

empty :: Interface
empty = Interface Map.empty Map.empty
