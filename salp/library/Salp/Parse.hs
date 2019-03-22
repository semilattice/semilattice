{-# LANGUAGE OverloadedStrings #-}

module Salp.Parse
  ( -- * Parser
    Parser

    -- * Source files
  , sourceFileP
  , importP

    -- * Definitions
  , topLevelP
  , methodP

    -- * Expressions
  , statementP
  , expressionP
  , typeP

    -- * Miscellaneous
  , namespaceNameP
  , parameterP
  , fieldP
  ) where

import Salp.Ast
import Salp.Lex

import Control.Applicative ((<|>), many)
import Data.ByteString (ByteString)
import Data.Foldable (foldl')

import qualified Text.Megaparsec as A
import qualified Text.Megaparsec.Byte as A

--------------------------------------------------------------------------------
-- Source files

sourceFileP :: Parser SourceFile
sourceFileP = do
  whitespace

  versionK
  _ <- token $ A.string "\"salp-0.0.0\""
  semicolonT

  imports <- many importP

  topLevels <- many topLevelP

  A.eof

  pure $ SourceFile imports topLevels

importP :: Parser Import
importP = classImportP
  where
  classImportP :: Parser Import
  classImportP = do
    fromK

    namespace <- namespaceNameP

    importK
    classK

    class_ <- identifierT

    semicolonT

    pure $ ClassImport namespace class_

--------------------------------------------------------------------------------
-- Definitions

topLevelP :: Parser TopLevel
topLevelP = namespaceP <|> classP <|> recordP
  where
  namespaceP :: Parser TopLevel
  namespaceP = do
    namespaceK

    namespace <- identifierT

    leftBraceT
    body <- many topLevelP
    rightBraceT

    pure $ Namespace namespace body

  classP :: Parser TopLevel
  classP = do
    classK

    name <- identifierT

    leftParenthesisT
    constructorFields <- parameterP `A.sepBy` commaT
    rightParenthesisT

    leftBraceT
    body <- many methodP
    rightBraceT

    pure $ Class name constructorFields body

  recordP :: Parser TopLevel
  recordP = do
    recordK

    name <- identifierT

    leftBraceT
    fields <- many fieldP
    rightBraceT

    pure $ Record name fields

methodP :: Parser Method
methodP = do
  programK

  name <- identifierT

  leftParenthesisT
  parameters <- parameterP `A.sepBy` commaT
  rightParenthesisT

  returnType <- typeP

  leftBraceT
  body <- many statementP
  rightBraceT

  pure $ Method name parameters returnType body

--------------------------------------------------------------------------------
-- Expressions

statementP :: Parser Statement
statementP = returnStatementP
  where
  returnStatementP :: Parser Statement
  returnStatementP = do
    returnK
    argument <- expressionP
    semicolonT
    pure $ ReturnStatement argument

expressionP :: Parser Expression
expressionP = methodCallExpressionP primitiveExpressionP
  where
  methodCallExpressionP :: Parser Expression -> Parser Expression
  methodCallExpressionP next = do

    receiver <- next

    methodCalls <- many $ do
      periodT

      method <- identifierT

      leftParenthesisT
      arguments <- expressionP `A.sepBy` commaT
      rightParenthesisT

      pure $ \receiver' -> MethodCallExpression receiver' method arguments

    pure $ foldl' (flip ($)) receiver methodCalls

  primitiveExpressionP :: Parser Expression
  primitiveExpressionP = symbolExpressionP
    where
    symbolExpressionP :: Parser Expression
    symbolExpressionP = SymbolExpression <$> identifierT

typeP :: Parser Type
typeP = stringTypeP <|> symbolTypeP
  where
  stringTypeP :: Parser Type
  stringTypeP = StringType <$ stringK

  symbolTypeP :: Parser Type
  symbolTypeP = SymbolType <$> identifierT

--------------------------------------------------------------------------------
-- Miscellaneous

namespaceNameP :: Parser [ByteString]
namespaceNameP = rootP <|> notRootP
  where
  rootP :: Parser [ByteString]
  rootP = [] <$ rootK

  notRootP :: Parser [ByteString]
  notRootP = identifierT `A.sepBy1` periodT

parameterP :: Parser Parameter
parameterP = do
  name  <- identifierT
  type_ <- typeP
  pure $ Parameter name type_

fieldP :: Parser Field
fieldP = do
  name  <- identifierT
  type_ <- typeP
  semicolonT
  pure $ Field name type_
