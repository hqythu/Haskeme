module Arithmetic where

import Control.Monad.Except
import Text.ParserCombinators.Parsec

import Definition

primitives :: [(String, [SchemeVal] -> ThrowsError SchemeVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop (/)),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              ("<=", numBoolBinop (<=)),
              (">", numBoolBinop (>)),
              (">=", numBoolBinop (>=)),
              ("not", boolBoolUnop (not)),
              ("and", boolBoolBinop (&&)), 
              ("or", boolBoolBinop (||))]

apply :: String -> [SchemeVal] -> ThrowsError SchemeVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

numericBinop :: (Double -> Double -> Double) -> [SchemeVal] -> ThrowsError SchemeVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: SchemeVal -> ThrowsError Double
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                           if null parsed 
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "Number" notNum

boolBinop :: (SchemeVal -> ThrowsError a) -> (a -> a -> Bool) -> [SchemeVal] -> ThrowsError SchemeVal
boolBinop unpacker op args =
    if length args /= 2
    then throwError $ NumArgs 2 args
    else do 
        left <- unpacker $ args !! 0
        right <- unpacker $ args !! 1
        return $ Bool $ left `op` right

boolUnop :: (SchemeVal -> ThrowsError a) -> (a -> Bool) -> [SchemeVal] -> ThrowsError SchemeVal
boolUnop unpacker op args =
    if length args /= 1
    then throwError $ NumArgs 1 args
    else do 
        val <- unpacker $ args !! 0
        return $ Bool $ op val

unpackStr :: SchemeVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: SchemeVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool
boolBoolUnop = boolUnop unpackBool