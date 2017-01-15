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

-- runRepl :: IO ()
-- runRepl = runInputT defaultSettings loop
--     where
--         loop :: InputT IO ()
--         loop = do
--             minput <- getInputLine "Haskeme>>> "
--             env <- nullEnv
--             case minput of
--                 Nothing -> outputStrLn "EOF detected, program exit"
--                 Just ":q" -> outputStrLn "Program exit"
--                 Just input -> do
--                     (evalString nullEnv input) >>= toInputT >>= outputStrLn
--                     loop

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   result <- prompt
   if pred result
      then return ()
      else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = nullEnv >>= (until_ (== "quit") (readPrompt "Haskeme>>> ")) . evalAndPrint

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

main :: IO ()
main = do
    args <- getArgs
    case length args of 
        0 -> runRepl
        1 -> runOne $ args !! 0
        otherwise -> putStrLn "Program takes only 0 or 1 argument"