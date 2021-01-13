module Value
    ( LispVal(..),
      LispError(..),
      IOThrowsError,
      ThrowsError,
      trapError,
      extractValue,
      liftThrows,
      Env,
      nullEnv,
      getVar,
      setVar,
      defineVar,
      bindVars
    ) where

import           Control.Monad.Except
import           Data.IORef
import           System.IO
import           Text.ParserCombinators.Parsec (ParseError)

data LispVal = Bool Bool
    | Number Integer
    | Character Char
    | String String
    | Symbol String
    | Pair [LispVal] LispVal
    | List [LispVal]
    | Error String
    | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
    | Func { params  :: [String],
             vararg  :: (Maybe String),
             body    :: [LispVal],
             closure :: Env }
    | IOFunc ([LispVal] -> IOThrowsError LispVal)
    | Port Handle

showVal (Bool b) = case b of
    True  -> "#t"
    False -> "#f"
showVal (Number n) = show n
showVal (Character c) = show c
showVal (String s) = "\"" ++ s ++ "\""
showVal (Symbol s) = s
showVal (Pair fst snd) =
    "(" ++ (unwords $ map showVal fst) ++ " . " ++ showVal snd ++ ")"
showVal (List vals) = "(" ++ (unwords $ map showVal vals) ++ ")"
showVal (Error err) = "ERROR: " ++ show err
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func params vararg body closure) =
    "(lambda (" ++ unwords (map show params) ++
        (case vararg of
            Nothing  -> ""
            Just arg -> " . " ++ arg) ++ ") ...)"
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _) = "<IO port>"

instance Show LispVal where show = showVal


-- error handling

data LispError = UnboundDef String
    | NumArgs Int Int -- NumArgs expected actual
    | BadArgument
    | Parser ParseError
    | NotSupported String

showErr (UnboundDef def) = "unbound definition: " ++ show def
showErr (NumArgs expected actual) = "incorrect # of args: expected=" ++ show expected ++ "; found=" ++ show actual
showErr (BadArgument) = "bad argument!"
showErr (Parser err) = "parser error: " ++ show err
showErr (NotSupported datum) = "item not supported: " ++ datum

instance Show LispError where show = showErr

type ThrowsError = Either LispError
type IOThrowsError = ExceptT LispError IO

trapError action = catchError action (return . Error . show)

extractValue :: ThrowsError a -> a
extractValue (Right x) = x

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right x)  = return x


-- environment

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- lift $ readIORef envRef
    maybe (throwError $ UnboundDef var)
          (lift . readIORef)
          (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var val = do
    env <- lift $ readIORef envRef
    maybe (throwError $ UnboundDef var)
          (lift . (flip writeIORef val))
          (lookup var env)
    return val

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var val = do
    isDef <- lift $ isBound envRef var
    if isDef
        then setVar envRef var val >> return val
        else lift $ do
             env <- readIORef envRef
             valRef <- newIORef val
             writeIORef envRef ((var,valRef):env)
             return val

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef vars = readIORef envRef >>= extendEnv vars >>= newIORef
    where
        extendEnv vars env = liftM (++ env) (mapM addBinding vars)
        addBinding (var, val) = newIORef val >>= (\x -> return (var, x))

