module Bigstep where

import Syntax

aeval :: AExpr -> Integer
aeval expr =
  case expr of
    IntConst i             -> i
    ABinary Add      a1 a2 -> (aeval a1) + (aeval a2)
    ABinary Subtract a1 a2 -> (aeval a1) - (aeval a2)
    ABinary Multiply a1 a2 -> (aeval a1) * (aeval a2)
    ABinary Divide   a1 a2 -> (aeval a1) `div` (aeval a2)
    _ -> error "not implemented"


beval :: BExpr -> Bool
beval expr =
  case expr of
    BoolConst c           -> c
    Not b                 -> not (beval b)
    BBinary And     b1 b2 -> (beval b1) && (beval b2)
    BBinary Or      b1 b2 -> (beval b1) || (beval b2)
    RBinary Greater a1 a2 -> (aeval a1) > (aeval a2)
    RBinary Less    a1 a2 -> (aeval a1) < (aeval a2)

eval :: Stmt -> Stmt
eval stmt = 
  case stmt of
    Assign x i     -> Skip
    Seq (Skip, c2) -> c2
    Seq (c1, c2)   -> Seq (eval c1, c2)
    If b c1 c2     -> if (beval b) then eval c1
                                   else eval c2
    While b c      -> if (beval b) then Seq (c, While b c)
                                   else Skip
