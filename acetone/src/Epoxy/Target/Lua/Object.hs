-- |
-- Objects are to be linked.
module Epoxy.Target.Lua.Object
  ( Object (..)
  ) where

import Epoxy.Anf (Global)

import qualified Data.ByteString.Builder as BSB

data Object =
  Object
    { objectName :: Global
      -- TODO: Add field for globals that are referenced.
    , objectCode :: BSB.Builder }
