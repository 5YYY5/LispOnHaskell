module Utils
    ( getElement          -- теперь Either String a
    , sliceRange
    , isTokenKind
    , getLinePos
    , getColPos
    , allLeaf
    , isEqExprTree
    , isEqExpr
    , isEqToken
    , isEqTokenHelp
    , isFloat             -- Either String Bool
    , isInt               -- Either String Bool
    , getInt              -- Either String Int
    , getFloat            -- Either String Float
    , deepSeqTree         -- Either String (ExprTree a)
    , eval                -- ()
    , evalList            -- ()
    , showToken
    , showSExprVal
    , replaceLeafExpr
    , replaceLeafExprLst
    , flattenTree         -- Either String (ExprTree SExpr)
    , flattenTreeHelp     -- Either String [SExpr]
    , takeExprs
    , takeFuns
    ) where

import Types
import Data.List (foldl', intercalate)

-- | Безопасное получение элемента списка по индексу (поддерживает отрицательные индексы)
getElement :: [a] -> Int -> Either String a
getElement [] _ = Left "Error: Cannot get element from empty list"
getElement (x:xs) n
    | n == 0 = Right x
    | n > 0 = getElement xs (n - 1)
    | otherwise = getElement (x:xs) (length (x:xs) + n)

-- | Вырезает подсписок от first до last включительно (индексы могут быть отрицательными)
sliceRange :: [a] -> Int -> Int -> [a]
sliceRange [] _ _ = []
sliceRange (x:xs) first last
    | first < 0 = sliceRange (x:xs) (first + length (x:xs)) last
    | last < 0 = sliceRange (x:xs) first (last + length (x:xs))
    | first > last = sliceRange (x:xs) last first
    | last == 0 = [x]
    | first == 0 = x : sliceRange xs first (last - 1)
    | first > 0 = sliceRange xs (first - 1) (last - 1)

-- | Проверка типа токена
isTokenKind :: Token -> TokenKind -> Bool
isTokenKind (Token _ _ kind) targetKind = kind == targetKind

-- | Номер строки токена
getLinePos :: Token -> Int
getLinePos (Token line _ _) = line

-- | Номер колонки токена
getColPos :: Token -> Int
getColPos (Token _ col _) = col

-- | Проверяет, все ли элементы списка являются листьями
allLeaf :: [ExprTree a] -> Bool
allLeaf [] = True
allLeaf (LeafExpr _ : xl) = allLeaf xl
allLeaf (NodeExpr _ : _) = False

-- | Сравнение двух деревьев выражений
isEqExprTree :: ExprTree SExpr -> ExprTree SExpr -> Bool
isEqExprTree (NodeExpr lst1) (NodeExpr lst2)
    | length lst1 == length lst2 = foldl (&&) True (zipWith isEqExprTree lst1 lst2)
    | otherwise = False
isEqExprTree (LeafExpr expr1) (LeafExpr expr2) = isEqExpr expr1 expr2
isEqExprTree _ _ = False

-- | Сравнение двух S-выражений
isEqExpr :: SExpr -> SExpr -> Bool
isEqExpr (SAtomExpr lex1) (SAtomExpr lex2) = isEqToken lex1 lex2
isEqExpr (SNilExpr _) (SNilExpr _) = True
isEqExpr (SListExpr lst1) (SListExpr lst2)
    | length lst1 == length lst2 = foldl (&&) True (zipWith isEqExpr lst1 lst2)
    | otherwise = False
isEqExpr _ _ = False

-- | Сравнение двух токенов (без учёта позиции)
isEqToken :: Token -> Token -> Bool
isEqToken (Token _ _ t1) (Token _ _ t2)
    | t1 == t2 = isEqTokenHelp t1 t2
    | otherwise = False

-- | Вспомогательное сравнение типов токенов, содержащих данные
isEqTokenHelp :: TokenKind -> TokenKind -> Bool
isEqTokenHelp (SymVal str1) (SymVal str2) = str1 == str2
isEqTokenHelp (StrVal str1) (StrVal str2) = str1 == str2
isEqTokenHelp (IntVal n1) (IntVal n2) = n1 == n2
isEqTokenHelp (FloatVal f1) (FloatVal f2) = f1 == f2
isEqTokenHelp _ _ = True

-- | Проверка, является ли выражение числом с плавающей точкой
isFloat :: ExprTree SExpr -> Either String Bool
isFloat (LeafExpr (SAtomExpr (Token _ _ (FloatVal _)))) = Right True
isFloat (LeafExpr (SAtomExpr (Token _ _ (IntVal _)))) = Right False
isFloat _ = Left "Это не является числом"

-- | Проверка, является ли выражение целым числом
isInt :: ExprTree SExpr -> Either String Bool
isInt (LeafExpr (SAtomExpr (Token _ _ (FloatVal _)))) = Right False
isInt (LeafExpr (SAtomExpr (Token _ _ (IntVal _)))) = Right True
isInt _ = Left "Это не является числом"

-- | Извлечение целого числа из выражения
getInt :: ExprTree SExpr -> Either String Int
getInt (LeafExpr (SAtomExpr (Token _ _ (IntVal num)))) = Right num
getInt _ = Left "Это не является целым числом"

-- | Извлечение числа с плавающей точкой из выражения
getFloat :: ExprTree SExpr -> Either String Float
getFloat (LeafExpr (SAtomExpr (Token _ _ (FloatVal num)))) = Right num
getFloat _ = Left "Это не является числом с плавающей точкой"

-- | Глубокое форсирование вычисления дерева выражений
deepSeqTree :: [ExprTree a] -> Either String (ExprTree a)
deepSeqTree [] = Left "deepSeqTree: empty list"
deepSeqTree (x:xs) = Right (foldl' (\_ tree -> eval tree `seq` tree) x xs)

-- | Форсирование вычисления одного узла
eval :: ExprTree a -> ()
eval (LeafExpr x)   = x `seq` ()
eval (NodeExpr children) = evalList children `seq` ()

-- | Форсирование вычисления списка узлов
evalList :: [ExprTree a] -> ()
evalList [] = ()
evalList (t:ts) = eval t `seq` evalList ts

-- | Преобразование токена в строку для отладки
showToken :: Token -> String
showToken (Token _ _ LParen) = "("
showToken (Token _ _ RParen) = ")"
showToken (Token _ _ NilVal) = "nil"
showToken (Token _ _ TrueVal) = "T"
showToken (Token _ _ (SymVal str)) = str
showToken (Token _ _ (StrVal str)) = "\"" ++ str ++ "\""
showToken (Token _ _ (IntVal n)) = show n
showToken (Token _ _ (FloatVal f)) = show f
showToken (Token _ _ QuoteSym) = "'"
showToken (Token _ _ DotSym) = "."
showToken (Token _ _ PlusOp) = "+"
showToken (Token _ _ MinusOp) = "-"
showToken (Token _ _ DivOp) = "/"
showToken (Token _ _ MulOp) = "*"
showToken (Token _ _ EqualOp) = "="
showToken (Token _ _ GreaterOp) = ">"
showToken (Token _ _ GreaterEqualOp) = ">="
showToken (Token _ _ LessOp) = "<"
showToken (Token _ _ LessEqualOp) = "<="
showToken (Token _ _ EndOfFile) = "<EOF>"

-- | Преобразование S-выражения в строку
showSExprVal :: SExpr -> String
showSExprVal (SAtomExpr tok) = showToken tok
showSExprVal (SNilExpr tok) = showToken tok
showSExprVal (SListExpr lst) = "(" ++ intercalate " " (map showSExprVal (sliceRange lst 0 (-2))) ++ ")"

-- | Замена листа с заданным именем на другое дерево
replaceLeafExpr :: ExprTree SExpr -> String -> ExprTree SExpr -> ExprTree SExpr
replaceLeafExpr (NodeExpr (x:xl)) str2 leaf = NodeExpr (replaceLeafExpr x str2 leaf : map (\tree -> replaceLeafExpr tree str2 leaf) xl)
replaceLeafExpr (LeafExpr (SAtomExpr (Token n m (SymVal str1)))) str2 leaf
    | str1 == str2 = leaf
    | otherwise = LeafExpr (SAtomExpr (Token n m (SymVal str1)))
replaceLeafExpr leafOld _ _ = leafOld

-- | Последовательная замена нескольких имён на соответствующие деревья
replaceLeafExprLst :: ExprTree SExpr -> [String] -> [ExprTree SExpr] -> ExprTree SExpr
replaceLeafExprLst body [] [] = body
replaceLeafExprLst body (name:ns) (leaf:ls) = replaceLeafExprLst (replaceLeafExpr body name leaf) ns ls

-- | Преобразование дерева в "плоский" список (свёртка Node в SList)
flattenTree :: ExprTree SExpr -> Either String (ExprTree SExpr)
flattenTree (LeafExpr sexpr) = Right (LeafExpr sexpr)
flattenTree (NodeExpr lst) =
    case flattenTreeHelp (map flattenTree lst) of
        Left err -> Left err
        Right sexprs -> Right (LeafExpr (SListExpr sexprs))

-- | Вспомогательная функция для flattenTree
flattenTreeHelp :: [Either String (ExprTree SExpr)] -> Either String [SExpr]
flattenTreeHelp [] = Right [SNilExpr (Token (-1) (-1) NilVal)]
flattenTreeHelp ((Right (LeafExpr sexpr)) : xl) =
    case flattenTreeHelp xl of
        Left err -> Left err
        Right rest -> Right (sexpr : rest)
flattenTreeHelp ((Left err) : _) = Left err
flattenTreeHelp _ = Left "Error: flattenTreeHelp: expected Leaf"

-- | Извлечение всех выражений (ExprDef) из списка определений
takeExprs :: [Definition] -> [ExprTree SExpr]
takeExprs [] = [(LeafExpr (SAtomExpr (Token (-1) (-1) EndOfFile)))]
takeExprs ((ExprDef expr):xl) = expr : takeExprs xl
takeExprs ((FuncDef _):xl) = takeExprs xl

-- | Извлечение всех определений функций (FuncDef)
takeFuns :: [Definition] -> [Definition]
takeFuns [] = []
takeFuns ((ExprDef _):xl) = takeFuns xl
takeFuns ((FuncDef fun):xl) = (FuncDef fun) : takeFuns xl