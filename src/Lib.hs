module Lib
    ( readExpr, eval, extractValue
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Except

import Definition
import Arithmetic
import Utils

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

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
parseList = liftM List (sepBy parseExpr spaces)

parseExpr :: Parser SchemeVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber
    <|> do
            char '('
            x <- parseList
            char ')'
            return x

eval :: SchemeVal -> ThrowsError SchemeVal
eval val@(Atom _) = return val
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "if", cond, trueExpr, falseExpr]) = do
    result <- eval cond
    case result of
        Bool True -> eval trueExpr
        otherwise -> eval falseExpr
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

readExpr :: String -> ThrowsError SchemeVal
readExpr input = case parse parseExpr "scheme" input of
     Left err -> throwError $ Parser err
     Right val -> return val
