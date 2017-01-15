module List where

import Control.Monad.Except
import Text.ParserCombinators.Parsec

import Definition

car :: [SchemeVal] -> ThrowsError SchemeVal
car [List (x : xs)]         = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [SchemeVal] -> ThrowsError SchemeVal
cdr [List (x : xs)]         = return $ List xs
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [SchemeVal] -> ThrowsError SchemeVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons badArgList = throwError $ NumArgs 2 badArgList