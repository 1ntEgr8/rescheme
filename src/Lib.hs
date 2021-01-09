module Lib
    ( eval,
      runRepl
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.IO
import Control.Monad
import Control.Monad.Except
import Data.Char

data LispVal = Bool Bool
    | Number Integer
    | Character Char
    | String String
    | Symbol String
    | Pair LispVal LispVal
    | List [LispVal] 

showVal (Bool b) = case b of
    True -> "#t"
    False -> "#f"
showVal (Number n) = show n
showVal (Character c) = show c
showVal (String s) = "\"" ++ s ++ "\""
showVal (Symbol s) = s
showVal (Pair fst snd) = 
    "(" ++ showVal fst ++ " . " ++ showVal snd ++ ")"
showVal (List vals) = "(" ++ (unwords $ map showVal vals) ++ ")"

instance Show LispVal where show = showVal

data LispError = UnboundDef String
    | NumArgs Int Int -- NumArgs expected actual
    | BadArgument
    | Parser ParseError

showErr (UnboundDef def) = "unbound definition: " ++ show def
showErr (NumArgs expected actual) = "incorrect # of args: expected=" ++ show expected ++ "; found=" ++ show actual
showErr (BadArgument) = "bad argument!"
showErr (Parser err) = "parser error: " ++ show err

instance Show LispError where show = showErr

spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Symbol "quote", x]

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parsePair :: Parser LispVal
parsePair = do
    head <- parseExpr
    spaces >> char '.' >> spaces
    tail <- parseExpr
    return $ Pair head tail

parseSymbol :: Parser LispVal
parseSymbol = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    return $ Symbol (first:rest)

escapeChar :: Parser Char
escapeChar = do
    char '\\'
    c <- oneOf "nrt\"\\"
    return $ case c of
        '\\' -> c
        '"' -> c
        'n' -> '\n'
        't' -> '\t'
        'r' -> '\r'

parseString :: Parser LispVal
parseString = do
    char '\"'
    s <- many $ escapeChar <|> noneOf "\""
    char '\"'
    return $ String s

parseNum :: Parser LispVal
parseNum = liftM (Number . read) $ many1 digit

-- TODO support r6rs spec completely
parseCharacter :: Parser LispVal
parseCharacter = do
    string "#\\"
    c <- anyChar
    return $ Character c

parseBool :: Parser LispVal
parseBool = do
    s <- string "#t" <|> string "#f" <|> string "#T" <|> string "#F"
    return $ case s of
        "#t" -> Bool True
        "#f" -> Bool False

parseExpr :: Parser LispVal
parseExpr = parseSymbol
    <|> parseCharacter
    <|> parseBool
    <|> parseQuoted
    <|> parseString
    <|> parseNum
    <|> do char '('
           x <- try parseList <|> parsePair
           char ')'
           return x

type ThrowsError = Either LispError

eval :: LispVal -> ThrowsError LispVal
eval n@(Number _) = return n
eval b@(Bool _) = return b
eval c@(Character _) = return c
eval s@(String _) = return s
eval (List [Symbol "quote", args]) = return args
eval (List ((Symbol func):args)) = mapM eval args >>= apply func

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ UnboundDef func) ($ args) $ (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("+", numBinOp (+))
             , ("-", numBinOp (-))
             , ("*", numBinOp (*))
             , ("/", numBinOp (div))
             , ("%", numBinOp (rem))]

numBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numBinOp func args = if length args < 2
                        then throwError $ NumArgs 2 (length args)
                        else mapM toNum args >>= return . Number . foldl1 func

toNum :: LispVal -> ThrowsError Integer
toNum (Number n) = return n
toNum (Character c) = return $ toInteger . ord $ c
toNum (String s) = let parsed = reads s in
                        if null parsed
                            then throwError $ BadArgument
                            else return $ fst $ parsed !! 0
toNum _ = throwError $ BadArgument

evalStr :: String -> LispVal
evalStr = undefined

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

runRepl :: IO ()
runRepl = do
    expr <- readPrompt ">> "
    if expr == "quit"
        then return ()
        else do 
            case parse parseExpr "rescheme" expr of
                Left err -> print err
                Right val -> print (eval val)
            runRepl
