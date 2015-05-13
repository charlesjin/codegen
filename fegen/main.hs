-- main module for the Crypto CodeGen Haskell utility
-- takes a prime as input and generates Go code
-- for fast scalar multiplcation routines

module Main where

  import Gen as G
  import Print as P
  import Param as M
  import Test as T
  import Data.List

  main :: IO ()
  main = do putStrLn "This is the Crypto CodeGen Haskell utility."
            putStrLn "[25519|7615|1271] [--weird]* [--unopt]*    for preconfigured examples."
            putStrLn "opttest                      [--unopt]*    for optimization tests."
            putStrLn "Anything else for manual config."
            x <- getLine
            if (isInfixOf "25519" x || isInfixOf "7615" x || isInfixOf "1271" x)
            then (putStr (P.printAbsyn (G.genAbsyn (M.genSampleParams x))))
            else (if (isInfixOf "opttest" x)
                  then (T.test x)
                  else (do 
                          putStrLn "Enter: base of prime."
                          b <- getLine
                          putStrLn "Enter: offset of prime."
                          o <- getLine
                          putStrLn "Enter: rep of prime, least significant block first, separated by spaces."
                          r <- getLine
                          putStrLn "Enter: any other flags."
                          f <- getLine
                          fegen b o r f))

  fegen :: String -> String -> String -> String -> IO ()
  fegen b o r f = (putStr (P.printAbsyn (G.genAbsyn (M.genParams b o r f))))
