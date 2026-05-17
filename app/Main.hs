module Main (main) where

import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Graphics.Gloss

import Types (Definition(..), ExprTree, SExpr(..))
import Lexer (tokenize)
import Parser (parseSyntax)
import Semantic (analyzeSemantics)
import Interpreter (my_evaluate)
import Utils (deepSeqTree, takeExprs, takeFuns)
import Astvisual (visualizeAST)

-- Отображение AST в окне Gloss
showAST :: ExprTree SExpr -> IO ()
showAST tree = do
    putStrLn "Displaying AST (close window to continue)..."
    display
        (InWindow "AST Viewer" (800, 600) (10, 10))
        white
        (visualizeAST tree)

-- Новая runProgram, принимающая уже распарсенные деревья
runProgram :: [Definition] -> [ExprTree SExpr] -> IO (Either String [Definition])
runProgram functional trees =
    case analyzeSemantics trees of
        Left err -> pure (Left err)
        Right defs -> do
            let fun = defs ++ functional
            evalRes <- my_evaluate fun fun
            case evalRes of
                Left err -> pure (Left err)
                Right result ->
                    case deepSeqTree (takeExprs result) of
                        Left err -> pure (Left err)
                        Right _ -> pure (Right result)

-- REPL с сохранением последнего AST
repl :: [Definition] -> Maybe (ExprTree SExpr) -> IO ()
repl functional lastAST = do
    putStr "Lisp-> "
    hFlush stdout
    inputLine <- getLine
    case inputLine of
        "esc" -> return ()
        "lookAST" -> do
            case lastAST of
                Nothing -> putStrLn "No AST to show."
                Just t  -> showAST t
            repl functional lastAST   -- возвращаемся в REPL
        _ -> do
            -- Парсим ввод для AST и для выполнения
            case tokenize inputLine >>= parseSyntax of
                Left err -> do
                    putStrLn err
                    repl functional lastAST   -- AST не меняется
                Right trees ->
                    let currentAST = case trees of
                                        []    -> Nothing
                                        (t:_) -> Just t
                    in do
                        evalResult <- runProgram functional trees
                        case evalResult of
                            Left err -> do
                                putStrLn err
                                repl functional currentAST
                            Right result ->
                                repl (takeFuns result) currentAST

-- Запуск файла с автоматической визуализацией AST
runFile :: FilePath -> IO ()
runFile path = do
    content <- readFile path
    case tokenize content >>= parseSyntax of
        Left err -> putStrLn err
        Right trees -> do
            -- Выполняем программу
            evalResult <- runProgram [] trees
            case evalResult of
                Left err -> putStrLn err
                Right _  -> return ()
            -- Показываем AST первого выражения (если есть)
            case trees of
                (t:_) -> showAST t
                _     -> return ()

selectMode :: IO ()
selectMode = do
    putStrLn "Select mode:"
    putStrLn "1) REPL"
    putStrLn "2) Run file"
    putStr "Choice: "
    hFlush stdout
    mode <- getLine
    case mode of
        "1" -> repl [] Nothing
        "2" -> do
            putStr "File path: "
            hFlush stdout
            path <- getLine
            runFile path
        _ -> putStrLn "Unknown mode. Use 1 or 2."

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> selectMode
        ["repl"] -> repl [] Nothing
        ["file", path] -> runFile path
        [path] -> runFile path
        _ -> putStrLn "Usage: my-project-exe [repl | file <path> | <path>]"