{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Epoxy.Target.Lua.Link
  ( link
  ) where

import Data.FileEmbed (embedFile)
import Epoxy.Target.Lua.Compile (mangleGlobal)
import Epoxy.Target.Lua.Object (Object (..))

import qualified Data.ByteString.Builder as BSB

link :: [Object] -> BSB.Builder
link os =
  -- TODO: Check that no name is defined twice.
  -- TODO: Check that all names that are referred to are also defined.
  prologue <>
  foldMap declare os <>
  foldMap define os <>
  epilogue

prologue :: BSB.Builder
prologue = BSB.byteString $(embedFile "src/Epoxy/Target/Lua/prologue.lua")

declare :: Object -> BSB.Builder
declare (Object x _) = "local " <> mangleGlobal x <> "\n"

define :: Object -> BSB.Builder
define = objectCode

epilogue :: BSB.Builder
epilogue = BSB.byteString $(embedFile "src/Epoxy/Target/Lua/epilogue.lua")
