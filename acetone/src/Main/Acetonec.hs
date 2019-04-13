{-# LANGUAGE OverloadedStrings #-}

module Main.Acetonec
  ( main
  ) where

import Acetone.Lower (lowerUnit)
import Acetone.Type.Check (checkAll)
import Acetone.Syntax (parse)
import Control.Monad ((<=<), join)
import System.Environment (getArgs)
import System.IO (stdout)

import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified Epoxy.Target.Lua.Compile as Lua (lowerUnit)
import qualified Epoxy.Target.Lua.Link as Lua (link)

main :: IO ()
main = do
  sources <- getArgs >>= traverse (either fail pure . parse <=< BSL.readFile)
  let unit = join sources
  case checkAll unit of
    Left err -> fail (show err)
    Right () ->
      let objects = Lua.lowerUnit (lowerUnit unit) in
      BSB.hPutBuilder stdout (Lua.link objects)
