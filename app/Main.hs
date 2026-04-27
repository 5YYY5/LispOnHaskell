module Main (main) where

import System.Environment (getArgs)
import System.IO (hFlush, stdout)

import Types (Definition(..))
import Lexer (tokenize)
import Parser (parseSyntax)
import Semantic (analyzeSemantics)
import Interpreter (my_evaluate)
import Utils (deepSeqTree, takeExprs, takeFuns)

runProgram :: [Definition] -> String -> IO (Either String [Definition])
runProgram functional program =
    case (tokenize program >>= parseSyntax >>= analyzeSemantics) of
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

repl :: [Definition] -> IO ()
repl functional = do
    putStr "Lisp->"
    hFlush stdout
    program <- getLine
    if program == "esc"
        then return ()
        else do
            evalResult <- runProgram functional program
            case evalResult of
                Left err -> do
                    putStrLn err
                    repl functional
                Right result -> repl (takeFuns result)

runFile :: FilePath -> IO ()
runFile path = do
    content <- readFile path
    evalResult <- runProgram [] content
    case evalResult of
        Left err -> putStrLn err
        Right _ -> pure ()

selectMode :: IO ()
selectMode = do
    putStrLn "Select mode:"
    putStrLn "1) REPL"
    putStrLn "2) Run file"
    putStr "Choice: "
    hFlush stdout
    mode <- getLine
    case mode of
        "1" -> repl []
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
        ["repl"] -> repl []
        ["file", path] -> runFile path
        [path] -> runFile path
        _ -> putStrLn "Usage: my-project-exe [repl | file <path> | <path>]"