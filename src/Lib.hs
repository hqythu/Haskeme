module Lib
    ( parseExpr
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

data SchemeVal 
    = Bool Bool
    | Number Double

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

parseExpr :: String -> String
parseExpr input = case parse parseNumber "lisp" input of
    Left err -> "No match: " ++ show err
    Right (Number x) -> show x
