module Definition where

import Text.ParserCombinators.Parsec
import Control.Monad.Except

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