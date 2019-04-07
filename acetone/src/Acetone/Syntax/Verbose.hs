module Acetone.Syntax.Verbose
  ( parse
  ) where

import Acetone.Ast (Unit)

import qualified Acetone.Syntax.Verbose.Lex as Lex
import qualified Acetone.Syntax.Verbose.Parse as Happy
import qualified Data.ByteString.Lazy as BSL

parse :: BSL.ByteString -> Either String Unit
parse = Right . Happy.parse . Lex.alexScanTokens
