module Main where

import System.IO
import System.Environment
import Lib

main :: IO ()
-- main = do 
--     (expr:_) <- getArgs
--     putStrLn (parseExpr expr)
main = getArgs >>= print . eval . readExpr . head