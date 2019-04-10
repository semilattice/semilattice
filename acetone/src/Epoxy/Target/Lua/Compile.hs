{-# LANGUAGE OverloadedStrings #-}

module Epoxy.Target.Lua.Compile
  ( -- * Infrastructure
    G
  , runG

    -- * Units
  , lowerUnit

    -- * Programs
  , lowerLet

    -- * Expressions
  , lowerExp
  , lowerRed

    -- * Values
  , lowerVal

    -- * Names
  , mangleLocal
  , mangleGlobal
  , mangleField
  , mangleDiscriminator
  ) where

import Epoxy.Anf

import Control.Monad.Trans.Writer (Writer, runWriter)
import Data.ByteString.Builder (Builder)
import Data.Foldable (for_, traverse_)
import Data.Tuple (swap)

import qualified Control.Monad.Writer.Class as Writer
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- Infrastructure

type G =
  Writer Builder

runG :: G a -> (Builder, a)
runG = swap . runWriter

define :: Builder -> Builder -> G ()
define x e = do
  Writer.tell $ "local " <> x <> " = " <> e <> "\n"

--------------------------------------------------------------------------------
-- Units

-- TODO: Instead of returning a Builder, return an object file which has all
-- the information necessary for linking safely.
lowerUnit :: Unit Val -> G ()
lowerUnit ds = do
  traverse_ declareG (Map.toList ds)
  traverse_ defineG (Map.toList ds)
  where
  declareG :: (Global, a) -> G ()
  declareG (x, _) = Writer.tell $ "local " <> mangleGlobal x <> "\n"

  defineG :: (Global, Let Val) -> G ()
  defineG (x, e) = do
    Writer.tell $ mangleGlobal x <> " = Rlazy(function(R_)\n"
    r <- lowerLet e
    Writer.tell $ "return " <> r <> "\n"
    Writer.tell $ "end)\n"

--------------------------------------------------------------------------------
-- Programs

lowerLet :: Let Val -> G Builder

lowerLet (Let x e₁ e₂) = do
  lowerExp (mangleLocal x) e₁
  lowerLet e₂

lowerLet LetRec{} =
  undefined

lowerLet (Return e) =
  pure $ lowerVal e

--------------------------------------------------------------------------------
-- Expressions

lowerExp :: Builder -> Exp Val -> G ()

lowerExp r (LambdaExp x e) = do
  Writer.tell $ "local " <> r <> " = function(" <> mangleLocal x <> ")\n"
  r' <- lowerLet e
  Writer.tell $ "return " <> r' <> "\n"
  Writer.tell $ "end\n"

lowerExp r (CaseExp e₁ cs) = do
  Writer.tell $ "local " <> r <> "\n"

  -- Identity, so we get valid syntax if cs is empty.
  Writer.tell $ "if false then\n"

  -- Generate elseif for each case.
  for_ (Map.toList cs) $ \(d, (x, e₂)) -> do

    -- Check discriminator.
    Writer.tell $ "elseif " <> lowerVal e₁ <> ".discriminator == " <>
                  "\"" <> mangleDiscriminator d <> "\" then\n"

    -- Assign bound variable.
    Writer.tell $ "local " <> mangleLocal x <> " = " <> lowerVal e₁ <> ".payload\n"

    -- Generate body.
    r' <- lowerLet e₂

    -- Remember result.
    Writer.tell $ r <> " = " <> r' <> "\n"

  -- End the if statement.
  Writer.tell $ "end\n"

lowerExp r (ReductionExp e) =
  lowerRed r e

lowerRed :: Builder -> Red Val -> G ()

lowerRed r (ApplyRed e₁ e₂) =
  let e₁' = lowerVal e₁ in
  let e₂' = lowerVal e₂ in
  define r $ e₁' <> "(" <> e₂' <> ")"

lowerRed r (RecordRed fs) =
  let field (k, v) = mangleField k <> " = " <> lowerVal v <> ",\n" in
  define r $ "{\n" <> foldMap field (Map.toList fs) <> "}"

lowerRed r (ProjectRed e f) =
  define r $ lowerVal e <> "." <> mangleField f

lowerRed r (InjectRed d e) =
  define r $ "{discriminator = \"" <> mangleDiscriminator d <> "\", " <>
             "payload = " <> lowerVal e <> "}"

lowerRed r (LazyRed e) =
  let e' = lowerVal e in
  define r $ "Rlazy(" <> e' <> ")"

--------------------------------------------------------------------------------
-- Values

lowerVal :: Val -> Builder
lowerVal (LocalVal x) = mangleLocal x
lowerVal (GlobalVal x) = mangleGlobal x <> "(Rarbitrary)"
lowerVal ConstantVal{} = undefined

--------------------------------------------------------------------------------
-- Names

mangleLocal :: Local -> Builder
mangleLocal (Local x) = "L" <> BSB.stringUtf8 (show x)

mangleGlobal :: Global -> Builder
mangleGlobal (Global x) = "G" <> mangle x

mangleField :: Field -> Builder
mangleField (Field x) = "F" <> mangle x

mangleDiscriminator :: Discriminator -> Builder
mangleDiscriminator (Discriminator x) = "D" <> mangle x

-- |
-- Replace @-@ by @_@ so that all Acetone identifier characters can occur in
-- Lua identifiers.
mangle :: BS.ByteString -> Builder
mangle = BSB.byteString . BS.map (\c -> if c == 0x2D then 0x5F else c)
