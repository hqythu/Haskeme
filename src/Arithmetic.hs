module Arithmetic where

import Definition

primitives :: [(String, [SchemeVal] -> SchemeVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop (/))]

apply :: String -> [SchemeVal] -> SchemeVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

numericBinop :: (Double -> Double -> Double) -> [SchemeVal] -> SchemeVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: SchemeVal -> Double
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Double, String)] in 
                           if null parsed 
                              then 0
                              else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0