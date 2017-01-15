module Lib
    ( readExpr, eval, extractValue, primitiveBindings
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Except
import Data.IORef

import Definition
import Arithmetic
import List
import Func
import Utils

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces1 :: Parser ()
spaces1 = skipMany1 space

parseAtom :: Parser SchemeVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of 
        "True" -> Bool True
        "False" -> Bool False
        _    -> Atom atom

parseInteger :: Parser SchemeVal
parseInteger = liftM (Number . \x -> read x :: Double) (many1 digit)

parseDouble :: Parser SchemeVal
parseDouble = do
    x <- many1 digit
    char '.'
    y <- many1 digit
    return $ Number (read (x ++ ['.'] ++ y) :: Double)

parseNumber :: Parser SchemeVal
parseNumber = try parseDouble <|> parseInteger

parseString :: Parser SchemeVal
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

parseList :: Parser SchemeVal
parseList = liftM List (sepBy parseExpr spaces1)

parseExpr :: Parser SchemeVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber
    <|> do
        char '('
        x <- parseList
        char ')'
        return x

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
              ("or", boolBoolBinop (||)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons)]

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
-- eval env val@(Atom _) = return val
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "if", cond, trueExpr, falseExpr]) = do
    result <- eval env cond
    case result of
        Bool True -> eval env trueExpr
        otherwise -> eval env falseExpr
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarArgs varargs env [] body
eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval env val@(List _) = return val
eval env badForm = throwError $ BadSpecialForm "Unrecognized form" badForm

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

readExpr :: String -> ThrowsError SchemeVal
readExpr input = case parse parseExpr "scheme" input of
     Left err -> throwError $ Parser err
     Right val -> return val

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
     where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)
