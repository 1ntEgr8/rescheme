module Lib
    ( runPrg
    , runRepl
    ) where

import System.IO
import Control.Monad.Except
import           Eval
import           Parser
import           Value


runPrg :: [String] -> IO ()
runPrg args = do
    env <- primitiveBindings >>= (flip bindVars [("args", List $ map String $ drop 1 args)])
    (runExceptT (trapError (x env))) >>= putStrLn . show
    where
        x env = eval env (List [Symbol "load", String (args !! 0)]) 

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") prompt . evalAndPrint

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

until_ :: (Monad m) => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
        then return ()
        else action result >> until_ pred prompt action

prompt :: IO String
prompt = flushStr "(scheme)>> " >> getLine

