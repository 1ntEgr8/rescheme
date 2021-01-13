module Parser
    ( readExpr,
      readExprList
    ) where

import           Control.Monad
import           Control.Monad.Except
import           Text.ParserCombinators.Parsec hiding (spaces)
import           Value

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "" input of
    Left err   -> throwError $ Parser err
    Right expr -> return expr

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

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
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
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
        '"'  -> c
        'n'  -> '\n'
        't'  -> '\t'
        'r'  -> '\r'

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
    try $ string "#\\"
    c <- anyChar
    return $ Character c

parseBool :: Parser LispVal
parseBool = do
    char '#'
    (char 't' >> return (Bool True)) <|>  (char 'f' >> return (Bool False))

parseExpr :: Parser LispVal
parseExpr = parseSymbol
    <|> parseString
    <|> try parseBool
    <|> try parseCharacter
    <|> parseQuoted
    <|> parseNum
    <|> do char '('
           x <- try parseList <|> parsePair
           char ')'
           return x

