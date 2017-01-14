module Main where

import System.IO
import System.Environment
import Control.Monad.Error
import System.Console.Haskeline

import Definition
import Lib

evalString :: String -> String
evalString expr = extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = do
    x <- return $ evalString expr
    putStrLn x

runRepl :: IO ()
runRepl = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "Haskeme>>> "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input -> do outputStrLn $ evalString input
                                loop

main :: IO ()
main = do
    args <- getArgs
    case length args of 
        0 -> runRepl
        1 -> evalAndPrint $ args !! 0
        otherwise -> putStrLn "Program takes only 0 or 1 argument"