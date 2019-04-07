{-# LANGUAGE OverloadedStrings #-}

module Acetone.Syntax
  ( parse
  ) where

import Acetone.Ast (Unit)
import Data.ByteString.Lazy (ByteString)

import qualified Acetone.Syntax.Verbose as Verbose
import qualified Data.ByteString.Lazy as BSL

parse :: ByteString -> Either String Unit
parse source =
  -- TODO: Properly parse #acetone and #syntax directives.
  let prologue = "#acetone v0\n#syntax verbose\n" in
  let (prologue', source') = BSL.splitAt (BSL.length prologue) source in
  if prologue /= prologue'
    then Left "invalid prologue"
    else Verbose.parse ("\n\n" <> source')
