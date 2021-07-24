module Syntax where

data BBinOp = And | Or deriving (Show)

data RBinOp = Greater | Less deriving (Show)

data ABinOp = Add
            | Subtract
            | Multiply
            | Divide
            deriving (Show)

data BExpr = BoolConst Bool
           | Not BExpr
           | BBinary BBinOp BExpr BExpr
           | RBinary RBinOp AExpr AExpr
           deriving (Show)

data AExpr = Var String
           | IntConst Integer
           | Neg AExpr
           | ABinary ABinOp AExpr AExpr
           deriving (Show)

data Stmt = Seq (Stmt, Stmt)
          | Assign String AExpr
          | If BExpr Stmt Stmt
          | While BExpr Stmt
          | Skip
          deriving (Show)
