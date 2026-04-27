module Main (main) where

import System.IO (hFlush, stdout)

import Types (Definition(..), ExprTree(..), SExpr(..))
import Lexer (tokenize)
import Parser (parseSyntax)
import Semantic (analyzeSemantics)
import Interpreter (my_evaluate)
import Utils (deepSeqTree, takeExprs, takeFuns, showSExprVal, flattenTree)

printTree :: ExprTree SExpr -> IO ()
printTree (LeafExpr sexpr) = putStrLn (showSExprVal sexpr)
printTree tree =
    case flattenTree tree of
        Left _ -> putStrLn (show tree)
        Right (LeafExpr sexpr) -> putStrLn (showSExprVal sexpr)
        Right other -> putStrLn (show other)

repl :: [Definition] -> IO ()
repl functional = do
    putStr "Lisp->"
    hFlush stdout
    program <- getLine
    if program == "esc"
        then return ()
        else do
            case (tokenize program >>= parseSyntax >>= analyzeSemantics) of
                Left err -> do
                    putStrLn err
                    repl functional
                Right defs -> do
                    let fun = defs ++ functional
                    evalRes <- my_evaluate fun fun
                    case evalRes of
                        Left err -> do
                            putStrLn err
                            repl functional
                        Right result -> do
                            case deepSeqTree (takeExprs result) of
                                Left err -> putStrLn err
                                Right _ -> return ()
                            mapM_ printTree (takeExprs result)
                            repl (takeFuns result)

main :: IO ()
main = repl []