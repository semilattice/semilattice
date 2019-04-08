{-# LANGUAGE OverloadedStrings #-}

module Main.Acetonec
  ( main
  ) where

import Acetone.Lower (lowerUnit)
import Acetone.Type.Check (checkAll)
import Acetone.Syntax (parse)
import Control.Monad ((<=<), join)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BSL

main :: IO ()
main = do
  sources <- getArgs >>= traverse (either fail pure . parse <=< BSL.readFile)
  let unit = join sources
  case checkAll unit of
    Left err -> print err
    Right () -> print (lowerUnit unit)
