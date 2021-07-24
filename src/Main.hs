module Main where

import Syntax
import Parser
import Interpreter

import qualified Data.Map as Map

import Control.Monad.IO.Class

import System.Environment

parseString :: String -> Stmt
parseString str =
  case parseStmt str of
    Left  e -> error $ show e
    Right r -> r

parseFile :: String -> IO Stmt
parseFile file =
  do program <- readFile file
     case parseStmt program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r

main :: IO ()
main = do
  args <- getArgs
  if length args < 1
     then putStrLn "No input file."
     else do
       ast <- parseFile $ head args
       print $ Map.toList $ interpret ast
