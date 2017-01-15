module Eval where

import Control.Monad
import Control.Monad.Except
import Data.IORef

import Definition
import Func

apply :: SchemeVal -> [SchemeVal] -> IOThrowsError SchemeVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
    if num params /= num args && varargs == Nothing
        then throwError $ NumArgs (num params) args
        else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where
        remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
            Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
            Nothing -> return env

eval :: Env -> SchemeVal -> IOThrowsError SchemeVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Symbol id) = getVar env id

eval env (List [Symbol "if", cond, trueExpr, falseExpr]) = do
    result <- eval env cond
    case result of
        Bool True -> eval env trueExpr
        otherwise -> eval env falseExpr

eval env (List [Symbol "set!", Symbol var, form]) =
    eval env form >>= setVar env var

eval env (List [Symbol "define", Symbol var, form]) =
    eval env form >>= defineVar env var

eval env (List (Symbol "define" : List (Symbol var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var

eval env (List [Symbol "let", Symbol var, val, expr]) = do
    newEnv <- liftIO $ readIORef env >>= newIORef
    x <- eval newEnv val
    defineVar newEnv var x
    eval newEnv expr

eval env (List (Symbol "lambda" : List params : body)) =
    makeNormalFunc env params body

eval env (List (Symbol "lambda" : varargs@(Symbol _) : body)) =
    makeVarArgs varargs env [] body

eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals

eval env val@(List _) = return val

eval env badForm = throwError $ BadSpecialForm "Unrecognized form" badForm
