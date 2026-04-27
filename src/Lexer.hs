module Lexer (tokenize, showTokens) where

import Types
import Data.Char (isSpace, isAlpha, isDigit, toUpper)

showTokens :: [Token] -> [Token]
showTokens = id

tokenize :: String -> Either String [Token]
tokenize str = scanTokens str 0 0

scanTokens :: String -> Int -> Int -> Either String [Token]
scanTokens [] line col = Right [(Token line col EndOfFile)]
scanTokens (x:xs:xl) line col
    | x == '>' && xs == '=' = (Token line col GreaterEqualOp :) <$> scanTokens xl line (col + 2)
    | x == '<' && xs == '=' = (Token line col LessEqualOp :) <$> scanTokens xl line (col + 2)
    | x == '(' && xs == ')' = (Token line col NilVal :) <$> scanTokens xl line (col + 2)
    | otherwise = scanHelp (x:xs:xl) line col        
scanTokens str line col = scanHelp str line col

scanHelp :: String -> Int -> Int -> Either String [Token]
scanHelp [] line col = Right [(Token line col EndOfFile)]
scanHelp (x:xl) line col
    | x == '\n' = scanTokens xl (line + 1) 0
    | isSpace x = scanTokens xl line (col + 1)
    | x == ';' = skipComment (x:xl) line col
    | isAlpha x || x == '_' = readSymbol (x:xl) [] line col col
    | isDigit x = readNumber (x:xl) [] line col col
    | x == '"' = readString xl [] line (col + 1) col
    | x == '(' = (Token line col LParen :) <$> scanTokens xl line (col + 1)
    | x == ')' = (Token line col RParen :) <$> scanTokens xl line (col + 1)
    | x == '\'' = (Token line col QuoteSym :) <$> scanTokens xl line (col + 1)
    | x == '.' = (Token line col DotSym :) <$> scanTokens xl line (col + 1)
    | x == '+' = (Token line col PlusOp :) <$> scanTokens xl line (col + 1)
    | x == '-' = (Token line col MinusOp :) <$> scanTokens xl line (col + 1)
    | x == '/' = (Token line col DivOp :) <$> scanTokens xl line (col + 1)
    | x == '*' = (Token line col MulOp :) <$> scanTokens xl line (col + 1)
    | x == '=' = (Token line col EqualOp :) <$> scanTokens xl line (col + 1)
    | x == '>' = (Token line col GreaterOp :) <$> scanTokens xl line (col + 1)
    | x == '<' = (Token line col LessOp :) <$> scanTokens xl line (col + 1)
    | otherwise = Left $ "Unexpected symbol: " ++ [x] ++ " line " ++ show line ++ ", col " ++ show col

skipComment :: String -> Int -> Int -> Either String [Token]
skipComment [] line col = Right [(Token line col EndOfFile)]
skipComment (x:xl) line col 
    | x /= '\n' = skipComment xl line (col + 1)
    | otherwise = scanTokens xl (line + 1) 0

readSymbol :: String -> String -> Int -> Int -> Int -> Either String [Token]
readSymbol [] [] line col _ = Right [(Token line col EndOfFile)]
readSymbol [] sym line col startCol = Right [Token line startCol (classifySymbol sym), Token line col EndOfFile]
readSymbol (x:xl) sym line col startCol
    | isAlpha x || isDigit x || x == '_' = readSymbol xl (sym ++ [x]) line (col + 1) startCol
    | otherwise = (Token line startCol (classifySymbol sym) :) <$> scanTokens (x:xl) line col

classifySymbol :: String -> TokenKind
classifySymbol sym
    | map toUpper sym == "T" = TrueVal
    | map toUpper sym == "NIL" = NilVal
    | otherwise = SymVal (map toUpper sym)

readString :: String -> String -> Int -> Int -> Int -> Either String [Token]
readString [] str _ _ _ = Left $ "Unterminated string: " ++ str ++ " (reached end of file)"
readString (x:xl) str line col startCol
    | x /= '"' = readString xl (str ++ [x]) line (col + 1) startCol
    | otherwise = (Token line startCol (StrVal str) :) <$> scanTokens xl line (col + 1)

readNumber :: String -> String -> Int -> Int -> Int -> Either String [Token]
readNumber [] [] line col _ = Right [(Token line col EndOfFile)]
readNumber [] digits line col startCol = Right [Token line startCol (IntVal (read digits)), Token line col EndOfFile]
readNumber (x:xl) digits line col startCol
    | isDigit x = readNumber xl (digits ++ [x]) line (col + 1) startCol
    | x == '.' = readFloatNum xl (digits ++ [x]) line (col + 1) startCol
    | otherwise = (Token line startCol (IntVal (read digits)) :) <$> scanTokens (x:xl) line col

readFloatNum :: String -> String -> Int -> Int -> Int -> Either String [Token]
readFloatNum [] [] _ _ _ = Left "Unexpected parse error"
readFloatNum [] digits line col startCol
    | last digits /= '.' = Right [Token line startCol (FloatVal (read digits)), Token line col EndOfFile]
    | otherwise = Left "Parse error: Unterminated number (reached end of file)"
readFloatNum (x:xl) digits line col startCol
    | isDigit x = readFloatNum xl (digits ++ [x]) line (col + 1) startCol
    | last digits == '.' = Left "Parse error: Unterminated number"
    | otherwise = (Token line startCol (FloatVal (read digits)) :) <$> scanTokens (x:xl) line col