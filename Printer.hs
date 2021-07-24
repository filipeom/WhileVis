module Printer where

import Syntax

stringOfBoolop :: BBinOp -> String
stringOfBoolop And = "&&"
stringOfBoolop Or  = "||"

stringOfRelop :: RBinOp -> String
stringOfRelop Greater = ">"
stringOfRelop Less    = "<"

stringOfBinop :: ABinOp -> String
stringOfBinop Add      = "+"
stringOfBinop Subtract = "-"
stringOfBinop Multiply = "*"
stringOfBinop Divide   = "/"

bexprToString :: BExpr -> String
bexprToString (BoolConst c)     = if c then "true" else "false"
bexprToString (Not expr)        = "!(" ++ (bexprToString expr) ++ ")"
bexprToString (BBinary b e1 e2) = 
  (bexprToString e1) ++ (stringOfBoolop b) ++ (bexprToString e2)
bexprToString (RBinary b a1 a2) =
  (aexprToString a1) ++ (stringOfRelop b) ++ (aexprToString a2)

aexprToString :: AExpr -> String
aexprToString (Var x)           = x
aexprToString (IntConst i)      = show i
aexprToString (Neg a)           = "-" ++ (aexprToString a)
aexprToString (ABinary b a1 a2) =
  (aexprToString a1) ++ (stringOfBinop b) ++ (aexprToString a2)

toString :: Stmt -> String
toString stmt =
  case stmt of
    Skip          -> ""
    Seq (c1, c2)  -> (toString c1) ++ ";\n" ++ (toString c2)
    Assign x a    -> x ++ " = " ++(aexprToString a)
    If b c1 c2    -> 
      "if " ++ (bexprToString b) ++ " { " ++
        (toString c1) ++ 
        " } else { " ++
        (toString c2) ++ 
        " }" 
    While b c     -> 
      "while " ++ (bexprToString b) ++ " { " ++
        (toString c) ++
      " }" 
