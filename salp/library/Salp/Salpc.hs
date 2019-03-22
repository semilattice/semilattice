module Salp.Salpc
  ( main
  ) where

import Data.Foldable (for_)
import Salp.Parse (sourceFileP)
import Salp.Translate (translate)
import System.IO (IOMode (..), withBinaryFile)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Text.Megaparsec as Megaparsec

main :: IO ()
main = do
  source <- ByteString.getContents
  result <- case Megaparsec.parse sourceFileP "<stdin>" source of
    Left err -> fail (Megaparsec.errorBundlePretty err)
    Right ast -> pure $ translate ast
  case result of
    Left err -> fail (show err)
    Right php ->
      for_ php $ \(filePath, builder) ->
        withBinaryFile filePath WriteMode $ \file -> do
          Builder.hPutBuilder file builder
