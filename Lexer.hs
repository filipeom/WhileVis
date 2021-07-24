module Lexer where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Token

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser languageDef
  where 
    languageDef = 
      emptyDef { Token.commentStart = "/*"
               , Token.commentEnd   = "*/"
               , Token.commentLine  = "//"
               , Token.identStart   = letter 
               , Token.identLetter  = alphaNum <|> oneOf "_'"
               , Token.reservedNames = [ "if"
                                       , "then"
                                       , "while"
                                       , "do"
                                       , "skip"
                                       , "true"
                                       , "false"
                                       , "not"
                                       , "and"
                                       , "or"
                                       ]
               , Token.reservedOpNames = [ "+", "-", "*", "/", ":="
                                         , "<", ">", "and", "or", "not"
                                         ]
               }

integer :: Parser Integer
integer = Token.integer    lexer

identifier  = Token.identifier lexer
parens     = Token.parens     lexer
semi       = Token.semi       lexer
whiteSpace = Token.whiteSpace lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer
