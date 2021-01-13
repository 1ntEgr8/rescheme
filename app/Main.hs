module Main where

import Lib
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    if null args
        then runRepl
        else runPrg $ args
