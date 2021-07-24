module Interpreter where

import Syntax

import Data.Map

type Valuation = Map String Integer

aeval :: AExpr -> Valuation -> Integer
aeval expr st =
  case expr of
    Var x                  -> st ! x
    IntConst i             -> i
    Neg a                  -> -(aeval a st)
    ABinary Add      a1 a2 -> (aeval a1 st) + (aeval a2 st)
    ABinary Subtract a1 a2 -> (aeval a1 st) - (aeval a2 st)
    ABinary Multiply a1 a2 -> (aeval a1 st) * (aeval a2 st)
    ABinary Divide   a1 a2 -> (aeval a1 st) `div` (aeval a2 st)

beval :: BExpr -> Valuation -> Bool
beval expr st =
  case expr of
    BoolConst c           -> c
    Not b                 -> not (beval b st)
    BBinary And     b1 b2 -> (beval b1 st) && (beval b2 st)
    BBinary Or      b1 b2 -> (beval b1 st) || (beval b2 st)
    RBinary Greater a1 a2 -> (aeval a1 st) > (aeval a2 st)
    RBinary Less    a1 a2 -> (aeval a1 st) < (aeval a2 st)

exec :: Stmt -> Valuation -> Valuation
exec stmt st =
  case stmt of
    Skip           -> st
    Assign x i     -> insert x (aeval i st) st
    Seq (c1, c2)   -> exec c2 (exec c1 st)
    If b c1 c2     -> if (beval b st) then exec c1 st
                                      else exec c2 st
    While b c      -> if (beval b st) then exec (Seq (c, While b c)) st
                                      else st

interpret :: Stmt -> Valuation
interpret stmt = exec stmt empty
