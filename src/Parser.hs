module Parser (parseSyntax) where

import Types
import Utils (getElement, sliceRange, isTokenKind, getLinePos, getColPos)

-- Вспомогательная безопасная функция получения элемента
-- getElementSafe :: [Token] -> Int -> Either String Token
-- getElementSafe tokens idx
--     | idx < 0 && idx >= -length tokens = Right (getElement tokens idx)
--     | idx >= 0 && idx < length tokens   = Right (getElement tokens idx)
--     | otherwise = Left $ "Index out of bounds: " ++ show idx

parseSyntax :: [Token] -> Either String [ExprTree SExpr]
parseSyntax tokens =
    case getElement tokens (-1) of
        Left err -> Left err
        Right lastToken ->
            if isTokenKind lastToken EndOfFile
                then parseHelper (sliceRange tokens 0 (-2))
                else Left "Syntax error: Expected end of file"

parseHelper :: [Token] -> Either String [ExprTree SExpr]
parseHelper [] = Right []
parseHelper tokens = do
    let taken = takeList tokens
    if null taken
        then Left "Parser error: takeList returned empty"
        else do
            sexpr <- readSExpr taken
            rest <- parseHelper (skipList tokens)
            Right (sexpr : rest)

readSExpr :: [Token] -> Either String (ExprTree SExpr)
readSExpr [] = Left "Syntax error: Expected at least one list"
readSExpr tokens = do
    first <- getElement tokens 0
    lastTok <- getElement tokens (-1)
    if isTokenKind first LParen && isTokenKind lastTok RParen
        then do
            let inner = sliceRange tokens 1 (-2)
            atom <- readAtom (takeList inner)
            exprs <- readExprList (skipList inner)
            Right (NodeExpr (atom : exprs))
        else if isTokenKind first LParen
            then Left $ "Syntax error: Expected ')' at " ++
                        show (getLinePos lastTok) ++ "::" ++ show (getColPos lastTok)
            else Left $ "Syntax error: Expected '(' at " ++
                        show (getLinePos first) ++ "::" ++ show (getColPos first)

readExprList :: [Token] -> Either String [ExprTree SExpr]
readExprList [] = Right []
readExprList (x:xs)
    | isTokenKind x QuoteSym = do
        let quoted = (Token (-1) (-1) LParen) : (x : takeList xs ++ [Token (-1) (-1) RParen])
        atom <- readAtom quoted
        rest <- readExprList (skipList xs)
        Right (atom : rest)
    | otherwise = do
        atom <- readAtom (takeList (x:xs))
        rest <- readExprList (skipList (x:xs))
        Right (atom : rest)

readAtom :: [Token] -> Either String (ExprTree SExpr)
readAtom [] = Left "Unexpected syntax error"
readAtom (x:xs)
    | isTokenKind x LParen = readSExpr (x:xs)
    | isTokenKind x NilVal = Right (LeafExpr (SNilExpr x))
    | otherwise = Right (LeafExpr (SAtomExpr x))

-- Чистые вспомогательные функции (не используют Either)
takeList :: [Token] -> [Token]
takeList [] = []
takeList (x:xs)
    | isTokenKind x LParen = takeListHelper xs [x] 1
    | otherwise = [x]

takeListHelper :: [Token] -> [Token] -> Int -> [Token]
takeListHelper [] [] _ = []
takeListHelper [] lst _ = lst
takeListHelper (x:xs) lst count
    | count == 0 = lst
    | isTokenKind x LParen = takeListHelper xs (lst ++ [x]) (count + 1)
    | isTokenKind x RParen = takeListHelper xs (lst ++ [x]) (count - 1)
    | otherwise = takeListHelper xs (lst ++ [x]) count

skipList :: [Token] -> [Token]
skipList [] = []
skipList (x:xs)
    | isTokenKind x LParen = skipListHelper xs 1
    | otherwise = xs

skipListHelper :: [Token] -> Int -> [Token]
skipListHelper [] _ = []
skipListHelper (x:xs) count
    | count == 0 = x:xs
    | isTokenKind x LParen = skipListHelper xs (count + 1)
    | isTokenKind x RParen = skipListHelper xs (count - 1)
    | otherwise = skipListHelper xs count