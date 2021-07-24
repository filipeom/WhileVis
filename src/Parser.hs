module Parser where

import Control.Monad (liftM)

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Expr
import qualified Text.Parsec.Token as Token

import Lexer
import Syntax

unary s t = Expr.Prefix (reservedOp s >> return (t))
binary s op assoc = Expr.Infix (reservedOp s >> return op) assoc

aOperators = [ [unary "-" Neg] 
             , [binary "*" (ABinary Multiply) Expr.AssocLeft,
                binary "/" (ABinary Divide  ) Expr.AssocLeft]
             , [binary "+" (ABinary Add     ) Expr.AssocLeft,
                binary "-" (ABinary Subtract) Expr.AssocLeft]
             ]

bOperators = [ [unary "not" Not]
             , [binary "and" (BBinary And) Expr.AssocLeft,
                binary "or"  (BBinary Or ) Expr.AssocLeft]
             ]

-- Expressions
aExpression :: Parser AExpr
aExpression = Expr.buildExpressionParser aOperators aTerm

bExpression :: Parser BExpr
bExpression = Expr.buildExpressionParser bOperators bTerm

aTerm =   parens aExpression
      <|> liftM Var identifier
      <|> liftM IntConst integer

bTerm =   parens bExpression
      <|> (reserved "true"  >> return (BoolConst True ))
      <|> (reserved "false" >> return (BoolConst False))
      <|> rExpression

rExpression =
  do a1 <- aExpression
     op <- relation
     a2 <- aExpression
     return $ RBinary op a1 a2

relation =   (reservedOp ">" >> return Greater)
         <|> (reservedOp "<" >> return Less)

ifStmt :: Parser Stmt
ifStmt =
  do reserved "if"
     cond <- bExpression
     reserved "then"
     stmt1 <- statement
     reserved "else"
     stmt2 <- statement
     return $ If cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt =
  do reserved "while"
     cond <- bExpression
     reserved "do"
     stmt <- statement
     return $ While cond stmt

assignStmt :: Parser Stmt
assignStmt =
  do var <- identifier
     reservedOp ":="
     expr <- aExpression
     return $ Assign var expr

skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> return Skip

sequenceOfStmt =
  do list <- (sepBy1 statement' semi)
     return $ if length list == 1 
                 then head list 
                 else foldr (\c acc -> Seq (c, acc)) Skip list

statement' :: Parser Stmt
statement' =   ifStmt
           <|> whileStmt
           <|> skipStmt
           <|> assignStmt

whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

statement :: Parser Stmt
statement =   parens statement
          <|> sequenceOfStmt

parseStmt :: String -> Either ParseError Stmt
parseStmt s = parse whileParser "" s
