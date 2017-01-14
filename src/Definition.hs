module Definition where

import Text.ParserCombinators.Parsec
import Control.Monad.Error

data SchemeVal 
    = Atom String
    | List [SchemeVal]
    | Bool Bool
    | Number Double
    | String String

unwordsList :: [SchemeVal] -> String
unwordsList = unwords . map showVal

showVal :: SchemeVal -> String
showVal (String str) = "\"" ++ str ++ "\""
showVal (Atom name) = name
showVal (Number num) = show num
showVal (Bool True) = "True"
showVal (Bool False) = "False"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"

instance Show SchemeVal where show = showVal

data SchemeError
    = NumArgs Integer [SchemeVal]
    | TypeMismatch String SchemeVal
    | Parser ParseError
    | BadSpecialForm String SchemeVal
    | NotFunction String String
    | UnboundVar String String
    | Default String

showError :: SchemeError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show SchemeError where show = showError

instance Error SchemeError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either SchemeError

trapError action = catchError action (return . show)