module Types
    ( TokenKind(..)
    , Token(..)
    , ExprTree(..)
    , SExpr(..)
    , Definition(..)
    , UserFunc(..)
    ) where

-- Token types
data TokenKind = LParen                -- (
               | RParen                -- )
               | NilVal
               | TrueVal
               | SymVal String
               | StrVal String
               | IntVal Int
               | FloatVal Float
               | QuoteSym              -- '
               | DotSym                -- .
               | PlusOp                -- +
               | MinusOp               -- -
               | DivOp                 -- /
               | MulOp                 -- *
               | EqualOp               -- =
               | GreaterOp             -- >
               | GreaterEqualOp        -- >=
               | LessOp                -- <
               | LessEqualOp           -- <=
               | EndOfFile
               deriving (Show)

instance Eq TokenKind where
    LParen == LParen = True
    RParen == RParen = True
    NilVal == NilVal = True
    TrueVal == TrueVal = True
    SymVal _ == SymVal _ = True
    StrVal _ == StrVal _ = True
    IntVal _ == IntVal _ = True
    FloatVal _ == FloatVal _ = True
    QuoteSym == QuoteSym = True
    DotSym == DotSym = True
    PlusOp == PlusOp = True
    MinusOp == MinusOp = True
    DivOp == DivOp = True
    MulOp == MulOp = True
    EqualOp == EqualOp = True
    GreaterOp == GreaterOp = True
    GreaterEqualOp == GreaterEqualOp = True
    LessOp == LessOp = True
    LessEqualOp == LessEqualOp = True
    EndOfFile == EndOfFile = True
    _ == _ = False

data Token = Token {
    linePos :: Int,
    colPos :: Int, 
    tokenKind :: TokenKind
} deriving (Show)

instance Eq Token where
    (Token _ _ k1) == (Token _ _ k2) = k1 == k2

-- Expression tree
data ExprTree a = LeafExpr a | NodeExpr [ExprTree a] deriving (Show)

-- S-expression
data SExpr = SAtomExpr Token
           | SNilExpr Token
           | SListExpr [SExpr]
           deriving (Show)

-- Definitions
data Definition = FuncDef UserFunc | ExprDef (ExprTree SExpr) deriving (Show)

data UserFunc = UserFunc {
    funcName :: String,
    funcParams :: [String],
    funcBody :: [(ExprTree SExpr)]
} deriving (Show)