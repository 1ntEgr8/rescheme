{-# LANGUAGE ExistentialQuantification #-}

module Eval
    ( eval, 
      evalStr,
      evalAndPrint,
      nullEnv,
      primitiveBindings
    ) where

import           Control.Monad.Except
import           Data.Char
import           Data.IORef
import           Parser
import           System.IO
import           Value

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalStr env expr >>= putStrLn

evalStr :: Env -> String -> IO String
evalStr env input = (runExceptT (trapError x)) >>= return . show . extractValue
    where
        x = (liftThrows $ readExpr input) >>= eval env

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env n@(Number _)                  = return n
eval env b@(Bool _)                    = return b
eval env c@(Character _)               = return c
eval env s@(String _)                  = return s
eval env (Symbol id) = getVar env id
eval env (List [Symbol "quote", args]) = return args
eval env (List [Symbol "set!", Symbol var, form]) = eval env form >>= setVar env var
eval env (List [Symbol "if", pred, conseq, alt]) =
    do result <- eval env pred
       case result of
           Bool False -> eval env alt
           otherwise  -> eval env conseq
eval env (List [Symbol "load", String filename]) =
    load filename >>= liftM last . mapM (eval env)
eval env (List [Symbol "define", Symbol var, form]) = eval env form >>= defineVar env var
eval env (List (Symbol "define": List (Symbol var: params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Symbol "define": Pair (Symbol var: params) varargs : body)) =
    makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Symbol "lambda": List params : body)) =
    makeNormalFunc env params body
eval env (List (Symbol "lambda": Pair params varargs : body)) =
    makeVarArgs varargs env params body
eval env (List (Symbol "lambda": varargs@(Symbol _) : body)) =
    makeVarArgs varargs env [] body
eval env (List (func:args))   = do
    func <- eval env func
    argVals <- mapM (eval env) args
    apply func argVals
eval env (Pair _ _) = throwError $ NotSupported "pair"
eval env val = throwError $ NotSupported (show val)

makeFunc varargs env params body = return $ Func (map show params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . show

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params vararg body closure) args =
    if length params /= length args && vararg == Nothing
        then throwError $ NumArgs (length params) (length args)
        else (lift $ bindVars closure $ zip params args) >>= bindVarargs vararg >>= evalBody
    where
        remainingArgs = drop (length params) args
        evalBody env = liftM last $ mapM (eval env) body
        bindVarargs arg env = case arg of
            Just name -> lift $ bindVars env [(name, List $ remainingArgs)]
            Nothing   -> return env
apply (IOFunc func) args = func args

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc PrimitiveFunc) primitives
                                               ++ map (makeFunc IOFunc) ioPrimitives )
    where
        makeFunc constr (var, func) = (var, constr func)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("+", numBinOp (+))
             , ("-", numBinOp (-))
             , ("*", numBinOp (*))
             , ("/", numBinOp (div))
             , ("%", numBinOp (rem))
             , ("=", numBoolBinOp (==))
             , ("<", numBoolBinOp (<))
             , (">", numBoolBinOp (>))
             , ("<=", numBoolBinOp (<=))
             , (">=", numBoolBinOp (>=))
             , ("&&", boolBoolBinOp (&&))
             , ("||", boolBoolBinOp (||))
             , ("string=?", strBoolBinOp (==))
             , ("string<?", strBoolBinOp (<))
             , ("string>?", strBoolBinOp (>))
             , ("string<=?", strBoolBinOp (<=))
             , ("string>=?", strBoolBinOp (>=))
             , ("car", car)
             , ("cdr", cdr)
             , ("cons", cons)
             , ("eq?", eqv)
             , ("eqv?", eqv)
             , ("equal?", equal) ]

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [ ("apply", applyProc)
               , ("open-input-file", makePort ReadMode)
               , ("open-output-file", makePort WriteMode)
               , ("close-input-file", closePort)
               , ("close-output-file", closePort)
               , ("read", readProc)
               , ("write", writeProc)
               , ("read-contents", readContents)
               , ("read-all", readAll) ]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func:args)       = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ lift $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = lift $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (lift $ hGetLine port) >>= (liftThrows . readExpr)

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = (lift $ hPrint port obj) >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ lift $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (lift $ readFile filename) >>= (liftThrows . readExprList)

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

numBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numBinOp func args = if length args < 2
                        then throwError $ NumArgs 2 (length args)
                        else mapM toNum args >>= return . Number . foldl1 func

boolBinOp :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinOp unpacker func args = if length args /= 2
                                    then throwError $ NumArgs 2 (length args)
                                    else do left <- unpacker (args !! 0)
                                            right <- unpacker (args !! 1)
                                            return $ Bool $ left `func` right

numBoolBinOp = boolBinOp toNum
strBoolBinOp = boolBinOp toStr
boolBoolBinOp = boolBinOp toBool

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)]   = return x
car [Pair (x:xs) _] = return x
car [badArg]        = throwError $ BadArgument
car badArg          = throwError $ BadArgument

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)]    = return $ List xs
cdr [Pair [_] x]     = return x
cdr [Pair (_: xs) x] = return $ Pair xs x
cdr [badArg]         = throwError $ BadArgument
cdr badArg           = throwError $ BadArgument

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []]      = return $ List [x]
cons [x, List xs]      = return $ List (x:xs)
cons [x, Pair xs xrem] = return $ Pair (x:xs) xrem
cons [x1, x2]          = return $ Pair [x1] x2
cons badArg            = throwError $ BadArgument

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Symbol arg1), (Symbol arg2)] = return $ Bool $ arg1 == arg2
eqv [(Pair xs x), (Pair ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) &&
                                                 (all eqvPair $ zip arg1 arg2)
    where
        eqvPair (x1, x2) = case eqv [x1,x2] of
                                Left _           -> False
                                Right (Bool val) -> val
eqv [_,_] = return $ Bool False
eqv badArg = throwError $ BadArgument

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
    eq1 <- liftM or $ mapM (unpackEquals arg1 arg2)
           [AnyUnpacker toNum, AnyUnpacker toStr, AnyUnpacker toBool]
    eq2Box <- eqv [arg1, arg2]
    return $ Bool $ (eq1 || let (Bool eq2) = eq2Box in eq2)

equal badArg = throwError $ BadArgument

-- unpacker helpers
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do unpacked1 <- unpacker arg1
                                                   unpacked2 <- unpacker arg2
                                                   return $ unpacked1 == unpacked2
                                                `catchError` (const $ return False)

toNum :: LispVal -> ThrowsError Integer
toNum (Number n) = return n
toNum (Character c) = return $ toInteger . ord $ c
toNum (String s) = let parsed = reads s in
                        if null parsed
                            then throwError $ BadArgument
                            else return $ fst $ parsed !! 0
toNum _ = throwError $ BadArgument

toStr :: LispVal -> ThrowsError String
toStr (String s) = return s
toStr (Number n) = return $ show n
toStr (Bool b)   = return $ show b
toStr _          = throwError $ BadArgument

toBool :: LispVal -> ThrowsError Bool
toBool (Bool b) = return $ b
toBool _        = throwError $ BadArgument
