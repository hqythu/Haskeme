module Definition where

import Text.ParserCombinators.Parsec
import Control.Monad.Except
import Data.IORef

data SchemeVal 
    = Atom String
    | List [SchemeVal]
    | Bool Bool
    | Number Double
    | String String

unwordsList :: [SchemeVal] -> String
unwordsList = unwords . map show

instance Show SchemeVal where
    show (String str) = "\"" ++ str ++ "\""
    show (Atom name) = name
    show (Number num) = show num
    show (Bool True) = "True"
    show (Bool False) = "False"
    show (List contents) = "(" ++ unwordsList contents ++ ")"

data SchemeError
    = NumArgs Integer [SchemeVal]
    | TypeMismatch String SchemeVal
    | Parser ParseError
    | BadSpecialForm String SchemeVal
    | NotFunction String String
    | UnboundVar String String
    | Default String

instance Show SchemeError where
    -- -- show :: SchemeError -> String
    show (UnboundVar message varname)  = message ++ ": " ++ varname
    show (BadSpecialForm message form) = message ++ ": " ++ show form
    show (NotFunction message func)    = message ++ ": " ++ show func
    show (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwordsList found
    show (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
    show (Parser parseErr)             = "Parse error at " ++ show parseErr

type ThrowsError = Either SchemeError

trapError action = catchError action (return . show)

type Env = IORef [(String, IORef SchemeVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ExceptT SchemeError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError SchemeVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
        (liftIO . readIORef)
        (lookup var env)

setVar :: Env -> String -> SchemeVal -> IOThrowsError SchemeVal
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting an unbound variable" var)
        (liftIO . (flip writeIORef value))
        (lookup var env)
    return value

defineVar :: Env -> String -> SchemeVal -> IOThrowsError SchemeVal
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef ((var, valueRef) : env)
            return value

bindVars :: Env -> [(String, SchemeVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)