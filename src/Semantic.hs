module Semantic (analyzeSemantics) where

import Types

analyzeSemantics :: [ExprTree SExpr] -> Either String [Definition]
analyzeSemantics exptrLst =
    case analyzeSemanticsHelp exptrLst of
        Left err -> Left err
        Right defs -> checkFun defs

analyzeSemanticsHelp :: [ExprTree SExpr] -> Either String [Definition]
analyzeSemanticsHelp [] = Right []
analyzeSemanticsHelp ((NodeExpr ((LeafExpr (SAtomExpr (Token _ _ (SymVal "DEFUN"))))
                                : (LeafExpr (SAtomExpr (Token _ _ (SymVal name))))
                                : (NodeExpr params)
                                : yl)) : xl)
    | not (null yl) =
        case getParams params of
            Left err -> Left err
            Right pnames ->
                case analyzeSemanticsHelp xl of
                    Left err -> Left err
                    Right rest -> Right (FuncDef (UserFunc name pnames yl) : rest)
analyzeSemanticsHelp ((NodeExpr ((LeafExpr (SAtomExpr (Token n m (SymVal "DEFUN")))) : _)) : yl) =
    Left $ "Invalid defun at " ++ show n ++ "::" ++ show m
analyzeSemanticsHelp (x : xl) =
    case analyzeSemanticsHelp xl of
        Left err -> Left err
        Right rest -> Right (ExprDef x : rest)

getParams :: [ExprTree SExpr] -> Either String [String]
getParams [] = Right []
getParams ((LeafExpr (SAtomExpr (Token _ _ (SymVal name)))) : xl) =
    case getParams xl of
        Left err -> Left err
        Right rest -> Right (name : rest)
getParams _ = Left "Defun error: invalid params"

checkFun :: [Definition] -> Either String [Definition]
checkFun [] = Right []
checkFun ((FuncDef (UserFunc name nparams body)) : xl) =
    case isFun xl name of
        Left err -> Left err
        Right True -> Left $ "Double defun a function '" ++ name ++ "'"
        Right False ->
            case checkFun xl of
                Left err -> Left err
                Right rest -> Right (FuncDef (UserFunc name nparams body) : rest)
checkFun (x : xl) =
    case checkFun xl of
        Left err -> Left err
        Right rest -> Right (x : rest)

isFun :: [Definition] -> String -> Either String Bool
isFun [] _ = Right False
isFun ((FuncDef (UserFunc name1 _ _)) : xl) name2
    | name1 == name2 = Right True
    | otherwise = isFun xl name2
isFun (_ : xl) name2 = isFun xl name2