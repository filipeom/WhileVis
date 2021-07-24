module Main where

import Syntax
import Parser
import Printer
import Bigstep

import Debug.Trace
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

exec :: Stmt -> Stmt
exec Skip = Skip
exec stmt = trace ("-- Trace: " ++ show stmt ++ "\n") $ exec (eval stmt)

main :: IO ()
main = do
  args <- getArgs
  if length args < 1
     then putStrLn "No input file."
     else do
       ast <- parseFile $ head args
       print $ exec ast
