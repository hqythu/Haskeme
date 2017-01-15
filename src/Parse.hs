module Parse where

import Text.ParserCombinators.Parsec
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

parseSymbol :: Parser SchemeVal
parseSymbol= do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let s = first:rest
    return $ case s of 
        "True" -> Bool True
        "False" -> Bool False
        _    -> Symbol s

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
parseExpr =
        parseSymbol
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

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

readExpr :: String -> ThrowsError SchemeVal
readExpr input = case parse parseExpr "scheme" input of
     Left err -> throwError $ Parser err
     Right val -> return val

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
     where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)
