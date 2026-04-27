module Main (main) where

import Interpreter (my_evaluate)
import Lexer (tokenize)
import Parser (parseSyntax)
import Semantic (analyzeSemantics)
import Types (Definition, ExprTree(..), SExpr(..))
import Utils (deepSeqTree, showSExprVal, takeExprs)

data Expected
    = ExpectRight String
    | ExpectLeftContains String

type TestCase = (String, String, Expected)

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

exprToString :: ExprTree SExpr -> String
exprToString (LeafExpr sexpr) = showSExprVal sexpr
exprToString _ = "<NODE>"

firstExprString :: [Definition] -> String
firstExprString defs =
    case takeExprs defs of
        [] -> "<EMPTY>"
        (x:_) -> exprToString x

assertCase :: TestCase -> IO Bool
assertCase (name, program, expected) = do
    res <- runProgram [] program
    case (expected, res) of
        (ExpectRight want, Right defs) ->
            let got = firstExprString defs
            in if got == want
                then pass name
                else failWith name ("expected " ++ show want ++ ", got " ++ show got)
        (ExpectLeftContains fragment, Left err) ->
            if fragment `contains` err
                then pass name
                else failWith name ("expected error containing " ++ show fragment ++ ", got " ++ show err)
        (ExpectRight want, Left err) ->
            failWith name ("expected " ++ show want ++ ", got error " ++ show err)
        (ExpectLeftContains fragment, Right defs) ->
            failWith name ("expected error containing " ++ show fragment ++ ", got " ++ show (firstExprString defs))

contains :: String -> String -> Bool
contains needle haystack = any (needle `prefixOf`) (tails haystack)

prefixOf :: String -> String -> Bool
prefixOf [] _ = True
prefixOf _ [] = False
prefixOf (x:xs) (y:ys) = x == y && prefixOf xs ys

tails :: String -> [String]
tails [] = [[]]
tails s@(_:xs) = s : tails xs

pass :: String -> IO Bool
pass name = do
    putStrLn ("[PASS] " ++ name)
    pure True

failWith :: String -> String -> IO Bool
failWith name msg = do
    putStrLn ("[FAIL] " ++ name ++ " - " ++ msg)
    pure False

runFileCase :: IO Bool
runFileCase = do
    content <- readFile "test/test"
    lineCount <- pure (length (filter (not . null) (lines content)))
    if lineCount /= 20
        then failWith "file has 20 expressions" ("expected 20, got " ++ show lineCount)
        else do
            res <- runProgram [] content
            case res of
                Left err -> failWith "execute test/test file" err
                Right _ -> pass "execute test/test file"

testCases :: [TestCase]
testCases =
    [ ("add integers", "(+ 1 2)", ExpectRight "3")
    , ("subtract integers", "(- 10 3)", ExpectRight "7")
    , ("multiply integers", "(* 4 5)", ExpectRight "20")
    , ("divide integers", "(/ 12 3)", ExpectRight "4.0")
    , ("numeric equal true", "(= 7 7)", ExpectRight "T")
    , ("greater true", "(> 9 2)", ExpectRight "T")
    , ("less false", "(< 3 1)", ExpectRight "nil")
    , ("quote long form", "(quote (1 2 3))", ExpectRight "(1 2 3)")
    , ("car quoted list", "(car '(9 8 7))", ExpectRight "9")
    , ("cdr quoted list", "(cdr '(9 8 7))", ExpectRight "(8 7)")
    , ("cons prepends", "(cons 1 '(2 3))", ExpectRight "(1 2 3)")
    , ("atom on integer", "(atom 123)", ExpectRight "T")
    , ("atom on list", "(atom '(1 2))", ExpectRight "nil")
    , ("null on nil", "(null nil)", ExpectRight "T")
    , ("not nil", "(not nil)", ExpectRight "T")
    , ("and true true", "(and t t)", ExpectRight "T")
    , ("or nil true", "(or nil t)", ExpectRight "T")
    , ("defun and call", "(defun add2 (x) (+ x 2))\n(add2 5)", ExpectRight "7")
    , ("cond chooses first branch", "(cond ((> 3 2) 42) (t 0))", ExpectRight "42")
    , ("unknown function error", "(unknown 1)", ExpectLeftContains "Not find function")
    ]

main :: IO ()
main = do
    results <- mapM assertCase testCases
    fileResult <- runFileCase
    let allOk = and (fileResult : results)
    if allOk
        then putStrLn "All tests passed."
        else error "Some tests failed."
