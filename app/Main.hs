module Main where

import System.IO
import System.Environment
import Control.Monad.Except
import System.Console.Haskeline

import Definition
import Lib

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

runRepl :: IO ()
runRepl = runInputT defaultSettings ((liftIO primitiveBindings) >>= loop)
    where
        loop :: Env -> InputT IO ()
        loop env = do
            minput <- getInputLine "Haskeme>>> "
            case minput of
                Nothing -> outputStrLn "EOF detected, program exit"
                Just ":q" -> outputStrLn "Program exit"
                Just input -> do
                    (liftIO (evalString env input)) >>= outputStrLn
                    loop env

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

main :: IO ()
main = do
    args <- getArgs
    case length args of 
        0 -> runRepl
        1 -> runOne $ args !! 0
        otherwise -> putStrLn "Program takes only 0 or 1 argument"
