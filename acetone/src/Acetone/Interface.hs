{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}

module Acetone.Interface
  ( Interface (..)
  ) where

import Acetone.Ast (Name)
import Acetone.Type (Type)
import Data.Map.Strict (Map)

-- |
-- The interface of a compilation unit contains all the information necessary
-- to compile dependants.
data Interface :: * where
  Interface
    :: { interfaceTypes  :: Map Name ([(Name, Type)], Type)
       , interfaceValues :: Map Name Type }
    -> Interface
  deriving stock (Eq, Show)
