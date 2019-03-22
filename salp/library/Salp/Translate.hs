{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Salp.Translate
  ( -- * High-level
    translate

    -- * Infrastructure
  , Translate
  , Error (..)
  , Symbol (..)
  , runTranslate

    -- * Definitions
  , classToFile
  , topLevelToClasses
  , methodToMethod

    -- * Expressions
  , statementToStatements
  , expressionToExpression
  , typeToPhpdoc

    -- * Miscellaneous
  , phpdoc
  ) where

import Salp.Ast

import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Builder (Builder)
import Data.Foldable (fold)
import Data.HashMap.Strict (HashMap)
import Data.Traversable (for)

import qualified Control.Monad.Error.Class as Error
import qualified Control.Monad.Reader.Class as Reader
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List

--------------------------------------------------------------------------------
-- High-level

translate :: SourceFile -> Either Error [(FilePath, Builder)]
translate (SourceFile imports topLevels) =
  runTranslate $
    let scope = [ (class_, ClassSymbol namespace class_)
                | ClassImport namespace class_ <- imports ] in
    Reader.local (HashMap.fromList scope <>) $ do
      classes <- fold <$> traverse topLevelToClasses topLevels
      pure [ classToFile namespace name builder
           | ((namespace, name), builder) <- classes ]

--------------------------------------------------------------------------------
-- Infrastructure

type Translate =
  ReaderT (HashMap ByteString Symbol) (Either Error)

data Error
  = InvalidSymbolError ByteString
  | UnknownSymbolError ByteString
  deriving stock (Eq, Show)

data Symbol
  = ConstructorFieldSymbol ByteString
  | ClassSymbol [ByteString] ByteString
  | VariableSymbol ByteString
  deriving stock (Eq, Show)

runTranslate :: Translate a -> Either Error a
runTranslate = runReaderT `flip` HashMap.empty

--------------------------------------------------------------------------------
-- Definitions

classToFile :: [ByteString] -> ByteString -> Builder -> (FilePath, Builder)
classToFile namespace name builder =
  let
    filePath :: FilePath
    filePath =
      fold (List.intersperse "/" (fmap a (namespace <> [name <> ".php"])))
        where a = ByteString.Char8.unpack

    builder' :: Builder
    builder' =
      let
        namespace'
          | null namespace = ""
          | otherwise = "namespace " <> fold (List.intersperse "\\" a) <> ";\n"
                          where a = fmap Builder.byteString namespace
      in
        "<?php\n" <>
        "declare(strict_types = 1);\n" <>
        namespace' <>
        builder

  in
    (filePath, builder')

topLevelToClasses :: TopLevel -> Translate [(([ByteString], ByteString), Builder)]

topLevelToClasses (Namespace namespace body) = do
  body' <- traverse topLevelToClasses body
  pure $ foldMap (fmap (first (first (<> [namespace])))) body'

topLevelToClasses (Class name constructorFields body) = do

  classFields' <-
    for constructorFields $ \(Parameter pname ptype) -> do
      t <- typeToPhpdoc ptype
      pure $ phpdoc [("var", t)] <>
             "private $" <> Builder.byteString pname <> ";\n"

  constructorParameterPhpdocs' <-
    for constructorFields $ \(Parameter pname ptype) -> do
      let n = Builder.byteString pname
      t <- typeToPhpdoc ptype
      pure ("param", t <> " $" <> n)

  let constructorParameters' =
        [ "$" <> Builder.byteString pname
        | Parameter pname _ <- constructorFields ]

  let constructorAssignments' =
        [ "$this->" <> Builder.byteString pname <> " = " <>
          "$" <> Builder.byteString pname <> ";\n"
        | Parameter pname _ <- constructorFields ]

  let constructor' =
        phpdoc constructorParameterPhpdocs' <>
        "public function __construct(" <>
        fold (List.intersperse ", " constructorParameters') <>
        ") {\n" <>
        fold constructorAssignments' <>
        "}\n"

  body' <-
    let scope = [ (pname, ConstructorFieldSymbol pname)
                | Parameter pname _ <- constructorFields ] in
    Reader.local (HashMap.fromList scope <>) $
      traverse methodToMethod body

  let builder =
        "final class " <> Builder.byteString name <> " {\n" <>
        fold classFields' <>
        constructor' <>
        fold body' <>
        "}\n"

  pure [(([], name), builder)]

topLevelToClasses (Record name fields) =
  let
    constructorParameters = [Parameter n t | Field n t <- fields]
    body = [] -- TODO: Generate accessors.
    class_ = Class name constructorParameters body
  in
    topLevelToClasses class_

methodToMethod :: Method -> Translate Builder
methodToMethod (Method name parameters returnType body) = do
  -- TODO: Generate type hints for parameters and return types.

  parameterPhpdocs <-
    for parameters $ \(Parameter pname ptype) -> do
      let n = Builder.byteString pname
      t <- typeToPhpdoc ptype
      pure ("param", t <> " $" <> n)

  returnTypePhpdoc <- do
    t <- typeToPhpdoc returnType
    pure ("return", t)

  let parameters' =
        [ "$" <> Builder.byteString pname
        | Parameter pname _ <- parameters ]

  body' <-
    let scope = [ (pname, VariableSymbol pname)
                | Parameter pname _ <- parameters ] in
    Reader.local (HashMap.fromList scope <>) $
      traverse statementToStatements body

  pure $ phpdoc (parameterPhpdocs <> [returnTypePhpdoc]) <>
         "public function " <> Builder.byteString name <> "(" <>
         fold (List.intersperse ", " parameters') <>
         ") {\n" <>
         fold body' <>
         "}\n"

--------------------------------------------------------------------------------
-- Expressions

statementToStatements :: Statement -> Translate Builder
statementToStatements (ReturnStatement argument) = do
  argument' <- expressionToExpression argument
  pure $ "return " <> argument' <> ";\n"

expressionToExpression :: Expression -> Translate Builder

expressionToExpression (MethodCallExpression receiver method arguments) = do
  receiver' <- expressionToExpression receiver
  arguments' <- traverse expressionToExpression arguments
  pure $ "(" <> receiver' <> "->" <> Builder.byteString method <> "(" <>
         fold (List.intersperse ", " arguments') <> "))"

expressionToExpression (SymbolExpression symbol) =
  Reader.asks (HashMap.lookup symbol) >>= \case

    Nothing            -> Error.throwError $ UnknownSymbolError symbol
    Just ClassSymbol{} -> Error.throwError $ InvalidSymbolError symbol

    Just (ConstructorFieldSymbol field) ->
      pure $ "($this->" <> Builder.byteString field <> ")"
    Just (VariableSymbol variable)      ->
      pure $ "$" <> Builder.byteString variable

typeToPhpdoc :: Type -> Translate Builder

typeToPhpdoc StringType =
  pure "string"

typeToPhpdoc (SymbolType symbol) =
  Reader.asks (HashMap.lookup symbol) >>= \case

    Nothing                       -> Error.throwError $ UnknownSymbolError symbol
    Just ConstructorFieldSymbol{} -> Error.throwError $ InvalidSymbolError symbol
    Just VariableSymbol{}         -> Error.throwError $ InvalidSymbolError symbol

    Just (ClassSymbol namespace class_) ->
      pure $ fullName namespace class_

--------------------------------------------------------------------------------
-- Miscellaneous

fullName :: [ByteString] -> ByteString -> Builder
fullName namespace name =
  "\\" <> fold (List.intersperse "\\" x)
  where x = fmap Builder.byteString (namespace <> [name])

phpdoc :: [(Builder, Builder)] -> Builder
phpdoc fields = prologue <> foldMap field fields <> epilogue
  where
  prologue = "/**\n"
  field (key, value) = " * @" <> key <> " " <> value <> "\n"
  epilogue =" */\n"
