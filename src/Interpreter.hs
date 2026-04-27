module Interpreter (my_evaluate, evalTree, evalArgs) where

import Types
import Utils
import Control.Monad.Except (ExceptT, runExceptT, throwError, liftEither)
import Control.Monad.IO.Class (liftIO)

nilExpr :: ExprTree SExpr
nilExpr = LeafExpr (SNilExpr (Token (-1) (-1) NilVal))

trueExpr :: ExprTree SExpr
trueExpr = LeafExpr (SAtomExpr (Token (-1) (-1) TrueVal))

type EvalM = ExceptT String IO

liftE :: Either String a -> EvalM a
liftE = liftEither

-- | Преобразование выражения в функцию (встроенную или пользовательскую)
getFunctionFromExpr :: ExprTree SExpr -> [Definition] -> EvalM ([ExprTree SExpr] -> EvalM (ExprTree SExpr))
getFunctionFromExpr (LeafExpr (SAtomExpr (Token n m t))) def
    | t == PlusOp         = pure (liftE . evalAdd)
    | t == MinusOp        = pure (liftE . evalSubtract)
    | t == MulOp          = pure (liftE . evalMultiply)
    | t == DivOp          = pure (liftE . evalDivide)
    | t == GreaterOp      = pure (liftE . evalGreater)
    | t == GreaterEqualOp = pure (liftE . evalGreaterOrEqual)
    | t == LessOp         = pure (liftE . evalLess)
    | t == LessEqualOp    = pure (liftE . evalLessOrEqual)
    | t == EqualOp        = pure (liftE . evalNumericEqual)
    | t == QuoteSym       = pure (liftE . evalQuote)
    | otherwise           = getBuiltinOrUserFunction (Token n m t) def
getFunctionFromExpr (LeafExpr x) _ = throwError $ "Error: " ++ show x ++ " is not a function name"
getFunctionFromExpr _ _ = throwError "Error: getFunctionFromExpr"

-- | Поиск встроенной или пользовательской функции по токену
getBuiltinOrUserFunction :: Token -> [Definition] -> EvalM ([ExprTree SExpr] -> EvalM (ExprTree SExpr))
getBuiltinOrUserFunction (Token n m (SymVal str)) def
    | str == "QUOTE" = pure (liftE . evalQuote)
    | str == "EVAL"  = pure (\args -> evalSpecialForm args def)
    | str == "CAR"   = pure (liftE . evalCar)
    | str == "CDR"   = pure (liftE . evalCdr)
    | str == "CONS"  = pure (liftE . evalCons)
    | str == "COND"  = pure (\args -> evalCond args def)
    | str == "EQ"    = pure (liftE . evalEq)
    | str == "EQL"   = pure (liftE . evalEql)
    | str == "ATOM"  = pure (liftE . evalAtom)
    | str == "AND"   = pure (\args -> evalAnd args def)
    | str == "OR"    = pure (\args -> evalOr args def)
    | str == "NOT"   = pure (liftE . evalNot)
    | str == "PRINT" = pure evalPrint
    | str == "NULL"  = pure (liftE . evalNull)
    | otherwise      = lookupUserFunction (Token n m (SymVal str)) def
getBuiltinOrUserFunction _ _ = throwError "getBuiltinOrUserFunction: invalid token"

-- | Вычисление простого выражения (лист или вызов функции)
evaluateSimpleExpr :: ExprTree SExpr -> [Definition] -> EvalM (ExprTree SExpr)
evaluateSimpleExpr (LeafExpr sexpr) _ = pure (LeafExpr sexpr)
evaluateSimpleExpr (NodeExpr []) _ = throwError "evaluateSimpleExpr: empty function call"
evaluateSimpleExpr (NodeExpr (fexpr:args)) def = do
    fun <- getFunctionFromExpr fexpr def
    suppliedArgs <-
        if isSpecialForm fexpr
            then pure args
            else evalArgs args def
    res <- fun suppliedArgs
    pure (checkNil res)

isSpecialForm :: ExprTree SExpr -> Bool
isSpecialForm (LeafExpr (SAtomExpr (Token _ _ QuoteSym))) = True
isSpecialForm (LeafExpr (SAtomExpr (Token _ _ (SymVal "QUOTE")))) = True
isSpecialForm (LeafExpr (SAtomExpr (Token _ _ (SymVal "EVAL")))) = True
isSpecialForm (LeafExpr (SAtomExpr (Token _ _ (SymVal "COND")))) = True
isSpecialForm (LeafExpr (SAtomExpr (Token _ _ (SymVal "AND")))) = True
isSpecialForm (LeafExpr (SAtomExpr (Token _ _ (SymVal "OR")))) = True
isSpecialForm _ = False

-- | Убирает лишний уровень списка (если результат – одноэлементный список с nil)
checkNil :: ExprTree SExpr -> ExprTree SExpr
checkNil (LeafExpr (SListExpr [(SNilExpr tok)])) = LeafExpr (SNilExpr tok)
checkNil x = x

-- | Основная функция интерпретации списка определений
my_evaluate :: [Definition] -> [Definition] -> IO (Either String [Definition])
my_evaluate prog def = runExceptT (my_evaluateM prog def)

my_evaluateM :: [Definition] -> [Definition] -> EvalM [Definition]
my_evaluateM [] _ = pure []
my_evaluateM ((FuncDef fun):xl) def = do
    rest <- my_evaluateM xl def
    pure (FuncDef fun : rest)
my_evaluateM ((ExprDef expr):xl) def = do
    computed <- evalTree expr def
    rest <- my_evaluateM xl def
    pure (ExprDef computed : rest)

-- | Вычисление одного дерева выражений
evalTree :: ExprTree SExpr -> [Definition] -> EvalM (ExprTree SExpr)
evalTree (LeafExpr sexpr) def = evaluateSimpleExpr (LeafExpr sexpr) def
evalTree (NodeExpr (x:xl)) def = do
    computedX <- evalTree x def
    evaluateSimpleExpr (NodeExpr (computedX : xl)) def

-- | Вычисление списка аргументов
evalArgs :: [ExprTree SExpr] -> [Definition] -> EvalM [ExprTree SExpr]
evalArgs lst def = mapM (\tree -> evalTree tree def) lst

-- | Поиск пользовательской функции
-- | Поиск пользовательской функции
lookupUserFunction :: Token -> [Definition] -> EvalM ([ExprTree SExpr] -> EvalM (ExprTree SExpr))
lookupUserFunction tok def = findUserFunction tok def def

findUserFunction :: Token -> [Definition] -> [Definition] -> EvalM ([ExprTree SExpr] -> EvalM (ExprTree SExpr))
findUserFunction (Token n m (SymVal str)) [] _ =
    throwError $ "Not find function '" ++ str ++ "' at " ++ show n ++ "::" ++ show m
findUserFunction (Token n m (SymVal str)) ((FuncDef (UserFunc name pnames body)):xl) def
    | str == name = pure (\argVals -> applyUserFunction (UserFunc name pnames body) argVals def)
    | otherwise   = findUserFunction (Token n m (SymVal str)) xl def
findUserFunction tok (_:xl) def = findUserFunction tok xl def

-- | Применение пользовательской функции
applyUserFunction :: UserFunc -> [ExprTree SExpr] -> [Definition] -> EvalM (ExprTree SExpr)
applyUserFunction (UserFunc _ pnames body) params def
    | length pnames /= length params = throwError "applyUserFunction: arity mismatch"
    | otherwise = do
        results <- mapM (\treeExpr -> evalTree (replaceLeafExprLst treeExpr pnames params) def) body
        liftE (deepSeqTree results)

-- | quote
evalQuote :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
evalQuote [x] = flattenTree x
evalQuote _ = Left "Wrong number of arguments: 'quote' is expected to contain 1 argument."

-- | eval
evalSpecialForm :: [ExprTree SExpr] -> [Definition] -> EvalM (ExprTree SExpr)
evalSpecialForm [x] def = evalTree x def
evalSpecialForm _ _ = throwError "Wrong number of arguments: 'eval' is expected to contain 1 argument."

-- | car
evalCar :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
evalCar [(LeafExpr (SListExpr (x:_)))] = Right (LeafExpr x)
evalCar [(LeafExpr sexpr)] = Left $ "Wrong type argument: It is expected that the 'car' " ++ show sexpr ++ " argument has the type 'list'."
evalCar lst
    | allLeaf lst = Left "Wrong number of arguments: 'car' is expected to contain 1 argument."
    | otherwise = Left "Error: evalCar"

-- | cdr
evalCdr :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
evalCdr [(LeafExpr (SListExpr []))]      = Right nilExpr
evalCdr [(LeafExpr (SListExpr [_]))]     = Right nilExpr
evalCdr [(LeafExpr (SListExpr (_:xs)))]  = Right (LeafExpr (SListExpr xs))
evalCdr [(LeafExpr (SNilExpr _))]        = Right nilExpr
evalCdr [(LeafExpr other)]               = Left $ "cdr: expected list, got " ++ show other
evalCdr lst
    | allLeaf lst = Left "cdr: wrong number of arguments (expected 1)"
    | otherwise   = Left "Error: evalCdr"

-- | cons
evalCons :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
evalCons ((LeafExpr sexpr):[(LeafExpr (SListExpr lst))]) = Right (LeafExpr (SListExpr (sexpr : lst)))
evalCons ((LeafExpr sexpr):[(LeafExpr (SNilExpr tok))]) = Right (LeafExpr (SListExpr (sexpr : [SNilExpr tok])))
evalCons ((LeafExpr sexpr):[(LeafExpr _)]) = Left "Wrong type argument: It is expected that the 'cons' second argument has the type 'list'."
evalCons lst
    | allLeaf lst = Left "Wrong number of arguments: 'cons' is expected to contain 2 arguments."
    | otherwise = Left "Error: evalCons"

-- | cond
evalCond :: [ExprTree SExpr] -> [Definition] -> EvalM (ExprTree SExpr)
evalCond [] _ = pure nilExpr
evalCond ((NodeExpr (pred:[expr])):xl) def = do
    predVal <- evalTree pred def
    if isEqExprTree trueExpr predVal
        then evalTree expr def
        else evalCond xl def
evalCond _ _ = throwError "incorrect count of args for cond"

-- | eq
evalEq :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
evalEq (exprTree1:[exprTree2]) =
    if isEqExprTree exprTree1 exprTree2
        then Right trueExpr
        else Right nilExpr
evalEq _ = Left "incorrect count of args for eq"

-- | eql (синоним eq)
evalEql :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
evalEql = evalEq

-- | atom
evalAtom :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
evalAtom [(LeafExpr (SListExpr _))] = Right nilExpr
evalAtom [_] = Right trueExpr
evalAtom _ = Left "incorrect count of args for atom"

-- | and
evalAnd :: [ExprTree SExpr] -> [Definition] -> EvalM (ExprTree SExpr)
evalAnd [] _ = pure trueExpr
evalAnd (x:xl) def = do
    xVal <- evalTree x def
    if isEqExprTree xVal nilExpr
        then pure nilExpr
        else evalAnd xl def

-- | or
evalOr :: [ExprTree SExpr] -> [Definition] -> EvalM (ExprTree SExpr)
evalOr [] _ = pure nilExpr
evalOr (x:xl) def = do
    xVal <- evalTree x def
    if isEqExprTree xVal nilExpr
        then evalOr xl def
        else pure trueExpr

-- | not
evalNot :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
evalNot [(LeafExpr (SNilExpr _))] = Right trueExpr
evalNot [_] = Right nilExpr
evalNot _ = Left "incorrect count of args for not"

-- | = (численное равенство)
evalNumericEqual :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
evalNumericEqual (x:[y]) = do
    xi <- isInt x
    yi <- isInt y
    xf <- isFloat x
    yf <- isFloat y
    case (xi, yi, xf, yf) of
        (True, True, _, _) -> do
            ix <- getInt x
            iy <- getInt y
            return $ if ix == iy then trueExpr else nilExpr
        (True, _, _, True) -> do
            ix <- getInt x
            fy <- getFloat y
            return $ if fromIntegral ix == fy then trueExpr else nilExpr
        (_, True, True, _) -> do
            fx <- getFloat x
            iy <- getInt y
            return $ if fx == fromIntegral iy then trueExpr else nilExpr
        (_, _, True, True) -> do
            fx <- getFloat x
            fy <- getFloat y
            return $ if fx == fy then trueExpr else nilExpr
        _ -> return nilExpr
evalNumericEqual _ = Left "incorrect count of args for ="

-- | +
evalAdd :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
evalAdd (x:[y]) = do
    xi <- isInt x
    yi <- isInt y
    xf <- isFloat x
    yf <- isFloat y
    case (xi, yi, xf, yf) of
        (True, True, _, _) -> do
            ix <- getInt x
            iy <- getInt y
            return (LeafExpr (SAtomExpr (Token (-1) (-1) (IntVal (ix + iy)))))
        (True, _, _, True) -> do
            ix <- getInt x
            fy <- getFloat y
            return (LeafExpr (SAtomExpr (Token (-1) (-1) (FloatVal (fromIntegral ix + fy)))))
        (_, True, True, _) -> do
            fx <- getFloat x
            iy <- getInt y
            return (LeafExpr (SAtomExpr (Token (-1) (-1) (FloatVal (fx + fromIntegral iy)))))
        (_, _, True, True) -> do
            fx <- getFloat x
            fy <- getFloat y
            return (LeafExpr (SAtomExpr (Token (-1) (-1) (FloatVal (fx + fy)))))
        _ -> Left "invalid arguments for +"
evalAdd _ = Left "incorrect count of args for +"

-- | -
evalSubtract :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
evalSubtract [x] = subtractIntPromoted [x] (LeafExpr (SAtomExpr (Token (-1) (-1) (IntVal 0))))
evalSubtract (x:xl) = subtractIntPromoted xl x
evalSubtract _ = Left "incorrect count of args for -"

subtractIntPromoted :: [ExprTree SExpr] -> ExprTree SExpr -> Either String (ExprTree SExpr)
subtractIntPromoted [y] x = do
    xi <- isInt x
    yi <- isInt y
    xf <- isFloat x
    yf <- isFloat y
    case (xi, yi, xf, yf) of
        (True, True, _, _) -> do
            ix <- getInt x
            iy <- getInt y
            return (LeafExpr (SAtomExpr (Token (-1) (-1) (IntVal (ix - iy)))))
        (True, _, _, True) -> do
            ix <- getInt x
            fy <- getFloat y
            return (LeafExpr (SAtomExpr (Token (-1) (-1) (FloatVal (fromIntegral ix - fy)))))
        (_, True, True, _) -> do
            fx <- getFloat x
            iy <- getInt y
            return (LeafExpr (SAtomExpr (Token (-1) (-1) (FloatVal (fx - fromIntegral iy)))))
        (_, _, True, True) -> do
            fx <- getFloat x
            fy <- getFloat y
            return (LeafExpr (SAtomExpr (Token (-1) (-1) (FloatVal (fx - fy)))))
        _ -> Left "invalid arguments for -"
subtractIntPromoted (y:yl) x = do
    xi <- isInt x
    yi <- isInt y
    xf <- isFloat x
    yf <- isFloat y
    case (xi, yi, xf, yf) of
        (True, True, _, _) -> do
            ix <- getInt x
            iy <- getInt y
            subtractIntPromoted yl (LeafExpr (SAtomExpr (Token (-1) (-1) (IntVal (ix - iy)))))
        (True, _, _, True) -> do
            ix <- getInt x
            fy <- getFloat y
            subtractFloatPromoted yl (LeafExpr (SAtomExpr (Token (-1) (-1) (FloatVal (fromIntegral ix - fy)))))
        (_, True, True, _) -> do
            fx <- getFloat x
            iy <- getInt y
            subtractFloatPromoted yl (LeafExpr (SAtomExpr (Token (-1) (-1) (FloatVal (fx - fromIntegral iy)))))
        (_, _, True, True) -> do
            fx <- getFloat x
            fy <- getFloat y
            subtractFloatPromoted yl (LeafExpr (SAtomExpr (Token (-1) (-1) (FloatVal (fx - fy)))))
        _ -> Left "invalid arguments for -"
subtractIntPromoted _ _ = Left "incorrect count of args for -"

subtractFloatPromoted :: [ExprTree SExpr] -> ExprTree SExpr -> Either String (ExprTree SExpr)
subtractFloatPromoted [y] x = do
    xf <- isFloat x
    yi <- isInt y
    case (xf, yi) of
        (True, True) -> do
            fx <- getFloat x
            iy <- getInt y
            return (LeafExpr (SAtomExpr (Token (-1) (-1) (FloatVal (fx - fromIntegral iy)))))
        (True, _) -> do
            fx <- getFloat x
            fy <- getFloat y
            return (LeafExpr (SAtomExpr (Token (-1) (-1) (FloatVal (fx - fy)))))
        _ -> Left "invalid arguments for -"
subtractFloatPromoted (y:yl) x = do
    xf <- isFloat x
    yi <- isInt y
    case (xf, yi) of
        (True, True) -> do
            fx <- getFloat x
            iy <- getInt y
            subtractFloatPromoted yl (LeafExpr (SAtomExpr (Token (-1) (-1) (FloatVal (fx - fromIntegral iy)))))
        (True, _) -> do
            fx <- getFloat x
            fy <- getFloat y
            subtractFloatPromoted yl (LeafExpr (SAtomExpr (Token (-1) (-1) (FloatVal (fx - fy)))))
        _ -> Left "invalid arguments for -"
subtractFloatPromoted _ _ = Left "incorrect count of args for -"

-- | *
evalMultiply :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
evalMultiply (x:[y]) = do
    xi <- isInt x
    yi <- isInt y
    xf <- isFloat x
    yf <- isFloat y
    case (xi, yi, xf, yf) of
        (True, True, _, _) -> do
            ix <- getInt x
            iy <- getInt y
            return (LeafExpr (SAtomExpr (Token (-1) (-1) (IntVal (ix * iy)))))
        (True, _, _, True) -> do
            ix <- getInt x
            fy <- getFloat y
            return (LeafExpr (SAtomExpr (Token (-1) (-1) (FloatVal (fromIntegral ix * fy)))))
        (_, True, True, _) -> do
            fx <- getFloat x
            iy <- getInt y
            return (LeafExpr (SAtomExpr (Token (-1) (-1) (FloatVal (fx * fromIntegral iy)))))
        (_, _, True, True) -> do
            fx <- getFloat x
            fy <- getFloat y
            return (LeafExpr (SAtomExpr (Token (-1) (-1) (FloatVal (fx * fy)))))
        _ -> Left "invalid arguments for *"
evalMultiply _ = Left "incorrect count of args for *"

-- | /
evalDivide :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
evalDivide (x:[y]) = do
    xi <- isInt x
    yi <- isInt y
    xf <- isFloat x
    yf <- isFloat y
    case (xi, yi, xf, yf) of
        (True, True, _, _) -> do
            ix <- getInt x
            iy <- getInt y
            return (LeafExpr (SAtomExpr (Token (-1) (-1) (FloatVal (fromIntegral ix / fromIntegral iy)))))
        (True, _, _, True) -> do
            ix <- getInt x
            fy <- getFloat y
            return (LeafExpr (SAtomExpr (Token (-1) (-1) (FloatVal (fromIntegral ix / fy)))))
        (_, True, True, _) -> do
            fx <- getFloat x
            iy <- getInt y
            return (LeafExpr (SAtomExpr (Token (-1) (-1) (FloatVal (fx / fromIntegral iy)))))
        (_, _, True, True) -> do
            fx <- getFloat x
            fy <- getFloat y
            return (LeafExpr (SAtomExpr (Token (-1) (-1) (FloatVal (fx / fy)))))
        _ -> Left "invalid arguments for /"
evalDivide _ = Left "incorrect count of args for /"

-- | >
evalGreater :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
evalGreater (x:[y]) = do
    xi <- isInt x
    yi <- isInt y
    xf <- isFloat x
    yf <- isFloat y
    case (xi, yi, xf, yf) of
        (True, True, _, _) -> do
            ix <- getInt x
            iy <- getInt y
            return $ if ix > iy then trueExpr else nilExpr
        (True, _, _, True) -> do
            ix <- getInt x
            fy <- getFloat y
            return $ if fromIntegral ix > fy then trueExpr else nilExpr
        (_, True, True, _) -> do
            fx <- getFloat x
            iy <- getInt y
            return $ if fx > fromIntegral iy then trueExpr else nilExpr
        (_, _, True, True) -> do
            fx <- getFloat x
            fy <- getFloat y
            return $ if fx > fy then trueExpr else nilExpr
        _ -> return nilExpr
evalGreater _ = Left "incorrect count of args for >"

-- | <
evalLess :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
evalLess (x:[y]) = do
    xi <- isInt x
    yi <- isInt y
    xf <- isFloat x
    yf <- isFloat y
    case (xi, yi, xf, yf) of
        (True, True, _, _) -> do
            ix <- getInt x
            iy <- getInt y
            return $ if ix < iy then trueExpr else nilExpr
        (True, _, _, True) -> do
            ix <- getInt x
            fy <- getFloat y
            return $ if fromIntegral ix < fy then trueExpr else nilExpr
        (_, True, True, _) -> do
            fx <- getFloat x
            iy <- getInt y
            return $ if fx < fromIntegral iy then trueExpr else nilExpr
        (_, _, True, True) -> do
            fx <- getFloat x
            fy <- getFloat y
            return $ if fx < fy then trueExpr else nilExpr
        _ -> return nilExpr
evalLess _ = Left "incorrect count of args for <"

-- | >=
evalGreaterOrEqual :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
evalGreaterOrEqual (x:[y]) = do
    xi <- isInt x
    yi <- isInt y
    xf <- isFloat x
    yf <- isFloat y
    case (xi, yi, xf, yf) of
        (True, True, _, _) -> do
            ix <- getInt x
            iy <- getInt y
            return $ if ix >= iy then trueExpr else nilExpr
        (True, _, _, True) -> do
            ix <- getInt x
            fy <- getFloat y
            return $ if fromIntegral ix >= fy then trueExpr else nilExpr
        (_, True, True, _) -> do
            fx <- getFloat x
            iy <- getInt y
            return $ if fx >= fromIntegral iy then trueExpr else nilExpr
        (_, _, True, True) -> do
            fx <- getFloat x
            fy <- getFloat y
            return $ if fx >= fy then trueExpr else nilExpr
        _ -> return nilExpr
evalGreaterOrEqual _ = Left "incorrect count of args for >="

-- | <=
evalLessOrEqual :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
evalLessOrEqual (x:[y]) = do
    xi <- isInt x
    yi <- isInt y
    xf <- isFloat x
    yf <- isFloat y
    case (xi, yi, xf, yf) of
        (True, True, _, _) -> do
            ix <- getInt x
            iy <- getInt y
            return $ if ix <= iy then trueExpr else nilExpr
        (True, _, _, True) -> do
            ix <- getInt x
            fy <- getFloat y
            return $ if fromIntegral ix <= fy then trueExpr else nilExpr
        (_, True, True, _) -> do
            fx <- getFloat x
            iy <- getInt y
            return $ if fx <= fromIntegral iy then trueExpr else nilExpr
        (_, _, True, True) -> do
            fx <- getFloat x
            fy <- getFloat y
            return $ if fx <= fy then trueExpr else nilExpr
        _ -> return nilExpr
evalLessOrEqual _ = Left "incorrect count of args for <="

-- | print
evalPrint :: [ExprTree SExpr] -> EvalM (ExprTree SExpr)
evalPrint [(LeafExpr sexpr)] = do
    liftIO (putStrLn (showSExprVal sexpr))
    pure (LeafExpr sexpr)
evalPrint lst
    | allLeaf lst = throwError "Wrong number of arguments: 'print' is expected to contain 1 argument."
    | otherwise = throwError ("Error: evalPrint" ++ show lst)

-- | null
evalNull :: [ExprTree SExpr] -> Either String (ExprTree SExpr)
evalNull [x] = evalEql [x, nilExpr]
evalNull _ = Left "incorrect count of args for null"