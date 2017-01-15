module Func where

import Definition

makeFunc :: Maybe String -> Env -> [SchemeVal] -> [SchemeVal] -> IOThrowsError SchemeVal
makeFunc varargs env params body = return $ Func (map show params) varargs body env

makeNormalFunc :: Env -> [SchemeVal] -> [SchemeVal] -> IOThrowsError SchemeVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: SchemeVal -> Env -> [SchemeVal] -> [SchemeVal] -> IOThrowsError SchemeVal
makeVarArgs = makeFunc . Just . show
