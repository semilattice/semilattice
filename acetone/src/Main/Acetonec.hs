{-# LANGUAGE OverloadedStrings #-}

module Main.Acetonec
  ( main
  ) where

import Acetone.Type.Check (checkAll)
import Acetone.Syntax (parse)
import Control.Monad ((<=<), join)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BSL

main :: IO ()
main = do
  sources <- getArgs >>= traverse (either fail pure . parse <=< BSL.readFile)
  let unit = join sources
  print $ checkAll unit
