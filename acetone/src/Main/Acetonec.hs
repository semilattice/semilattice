{-# LANGUAGE OverloadedStrings #-}

module Main.Acetonec
  ( main
  ) where

import Acetone.Ast

import Acetone.Type.Check (checkAll)

main :: IO ()
main =
  let
    -- IDENTIFICATION DIVISION
    -- VALUE-ID mul
    -- LINKAGE IS EXTERNAL
    -- SIGNATURE IS f32 -> f32 -> f32
    -- END-VALUE
    --
    -- IDENTIFICATION DIVISION
    -- VALUE-ID square
    -- LINKAGE IS EXTERNAL
    -- SIGNATURE IS f32 -> f32
    --
    -- CALCULUS DIVISION
    --   OVER x ABSTRACT
    --     mul x x
    --
    -- END-VALUE

    unit = [mulSig, squareSig, squareVal]

    mulSig =
      ValueSigDef (Name "mul") ExternalLinkage
        (ApplyTypeExp
          (ApplyTypeExp
            FunctionTypeExp
            (VariableTypeExp (Name "f32")))
          (ApplyTypeExp
            (ApplyTypeExp
              FunctionTypeExp
              (VariableTypeExp (Name "f32")))
            (VariableTypeExp (Name "f32"))))

    squareSig =
      ValueSigDef (Name "square") ExternalLinkage
        (ApplyTypeExp
          (ApplyTypeExp
            FunctionTypeExp
            (VariableTypeExp (Name "f32")))
          (VariableTypeExp (Name "f32")))

    squareVal =
      ValueDef (Name "square")
        (LambdaTermExp (Name "x")
          (ApplyTermExp
            (ApplyTermExp
              (VariableTermExp (Name "mul"))
              (VariableTermExp (Name "x")))
            (VariableTermExp (Name "x"))))

  in
    print $ checkAll unit
