module Astvisual (visualizeAST) where

import Data.Char (toUpper)
import Graphics.Gloss
import Types (ExprTree(..), SExpr(..), Token(..), TokenKind(..))

nodeRadius :: Float
nodeRadius   = 25

nodeDiameter :: Float
nodeDiameter = 2 * nodeRadius

horizontalGap :: Float
horizontalGap = 30

verticalGap :: Float
verticalGap   = 80

dotPitch :: Float
dotPitch = 2.2

dotRadius :: Float
dotRadius = 0.9

maxLabelLen :: Int
maxLabelLen = 12

visualizeAST :: ExprTree SExpr -> Picture
visualizeAST tree =
    let (nodes, edges) = layoutSubtree (0, 0) tree
    in pictures
        [ drawEdges edges
        , drawNodes nodes
        ]

subtreeWidth :: ExprTree SExpr -> Float
subtreeWidth (LeafExpr _)   = nodeDiameter
subtreeWidth (NodeExpr children) =
    max nodeDiameter
        ( sum (map subtreeWidth children)
        + fromIntegral (length children - 1) * horizontalGap
        )

layoutSubtree :: Point -> ExprTree SExpr -> ([(Point, String)], [(Point, Point)])
layoutSubtree rootPos (LeafExpr sexpr) =
    ( [(rootPos, showSExprToken sexpr)]
    , []
    )
layoutSubtree rootPos (NodeExpr children) =
    let thisNode  = (rootPos, "()")
        childY    = snd rootPos - verticalGap
        totalW    = sum (map subtreeWidth children) + fromIntegral (length children - 1) * horizontalGap
        startX    = fst rootPos - totalW / 2
        childPts  = placeChildren startX children childY
        childResults = zipWith layoutSubtree childPts children
        (childNodes, childEdges) = foldr (\(ns,es) (accN,accE) -> (ns ++ accN, es ++ accE))
                                         ([], []) childResults
        newEdges  = map (\cp -> (rootPos, cp)) childPts
    in (thisNode : childNodes, newEdges ++ childEdges)

placeChildren :: Float -> [ExprTree SExpr] -> Float -> [Point]
placeChildren _ [] _ = []
placeChildren leftX (c:cs) yBase =
    let w = subtreeWidth c
        x = leftX + w / 2
    in (x, yBase) : placeChildren (leftX + w + horizontalGap) cs yBase

showSExprToken :: SExpr -> String
showSExprToken (SAtomExpr (Token _ _ kind)) = tokenKindStr kind
showSExprToken (SNilExpr _)                 = "NIL"
showSExprToken (SListExpr _)                = "(list)"

tokenKindStr :: TokenKind -> String
tokenKindStr LParen          = "("
tokenKindStr RParen          = ")"
tokenKindStr NilVal          = "NIL"
tokenKindStr TrueVal         = "T"
tokenKindStr (SymVal s)      = s
tokenKindStr (StrVal s)      = "\"" ++ s ++ "\""
tokenKindStr (IntVal n)      = show n
tokenKindStr (FloatVal f)    = show f
tokenKindStr QuoteSym        = "'"
tokenKindStr DotSym          = "."
tokenKindStr PlusOp          = "+"
tokenKindStr MinusOp         = "-"
tokenKindStr DivOp           = "/"
tokenKindStr MulOp           = "*"
tokenKindStr EqualOp         = "="
tokenKindStr GreaterOp       = ">"
tokenKindStr GreaterEqualOp  = ">="
tokenKindStr LessOp          = "<"
tokenKindStr LessEqualOp     = "<="
tokenKindStr EndOfFile       = "<EOF>"

trimLabel :: String -> String
trimLabel s
    | length s <= maxLabelLen = s
    | otherwise               = take (maxLabelLen - 1) s ++ "~"

drawNode :: (Point, String) -> Picture
drawNode ((x, y), label) =
    Translate x y $ pictures
        [ Color (makeColor 0.8 0.9 1.0 1.0) $ circleSolid nodeRadius
        , Color black $ circle nodeRadius
        , Color black $ drawDotLabel (trimLabel label)
        ]

drawDotLabel :: String -> Picture
drawDotLabel str =
    let charW = 6 * dotPitch
        totalW = fromIntegral (length str) * charW
    in Translate (-totalW / 2 + dotPitch) (-3.5 * dotPitch) $
       pictures
           [ Translate (fromIntegral i * charW) 0 (drawDotChar c)
           | (i, c) <- zip ([0 ..] :: [Int]) str
           ]

drawDotChar :: Char -> Picture
drawDotChar c =
    pictures
        [ Translate (fromIntegral col * dotPitch) (fromIntegral row * dotPitch)
            $ circleSolid dotRadius
        | (col, row) <- glyphDots c
        ]

glyphDots :: Char -> [(Int, Int)]
glyphDots c = maybe (glyphDots '?') id (lookup (toUpper c) glyphTable)

glyphTable :: [(Char, [(Int, Int)])]
glyphTable =
    [ (' ', [])
    , ('?', fromGlyph ["..#..", "...#.", "..#..", ".#...", "..#..", ".....", "..#.."])
    , ('0', fromGlyph [".###.", "#...#", "#...#", "#...#", "#...#", "#...#", ".###."])
    , ('1', fromGlyph ["..#..", ".##..", "..#..", "..#..", "..#..", "..#..", ".###."])
    , ('2', fromGlyph [".###.", "#...#", "....#", "...#.", "..#..", ".#...", "#####"])
    , ('3', fromGlyph [".###.", "#...#", "....#", "..##.", "....#", "#...#", ".###."])
    , ('4', fromGlyph ["#...#", "#...#", "#...#", "#####", "....#", "....#", "....#"])
    , ('5', fromGlyph ["#####", "#....", "#....", "####.", "....#", "#...#", ".###."])
    , ('6', fromGlyph [".###.", "#...#", "#....", "####.", "#...#", "#...#", ".###."])
    , ('7', fromGlyph ["#####", "....#", "...#.", "..#..", ".#...", "#....", "#...."])
    , ('8', fromGlyph [".###.", "#...#", "#...#", ".###.", "#...#", "#...#", ".###."])
    , ('9', fromGlyph [".###.", "#...#", "#...#", ".####", "....#", "#...#", ".###."])
    , ('(', fromGlyph ["..#..", ".#...", "#....", "#....", "#....", ".#...", "..#.."])
    , (')', fromGlyph ["..#..", "...#.", "....#", "....#", "....#", "...#.", "..#.."])
    , ('+', fromGlyph [".....", "..#..", "..#..", "#####", "..#..", "..#..", "....."])
    , ('-', fromGlyph [".....", ".....", ".....", "#####", ".....", ".....", "....."])
    , ('*', fromGlyph [".....", "#...#", ".#.#.", "..#..", ".#.#.", "#...#", "....."])
    , ('/', fromGlyph ["....#", "....#", "...#.", "..#..", ".#...", "#....", "#...."])
    , ('=', fromGlyph [".....", ".....", "#####", ".....", "#####", ".....", "....."])
    , ('<', fromGlyph ["...#.", "..#..", ".#...", "#....", ".#...", "..#..", "...#."])
    , ('>', fromGlyph [".#...", "..#..", "...#.", "....#", "...#.", "..#..", ".#..."])
    , ('.', fromGlyph [".....", ".....", ".....", ".....", ".....", "..#..", "..#.."])
    , ('\'', fromGlyph ["..#..", "..#..", ".#...", ".....", ".....", ".....", "....."])
    , ('"', fromGlyph [".#.#.", ".#.#.", ".#.#.", ".....", ".....", ".....", "....."])
    , ('A', fromGlyph [".###.", "#...#", "#...#", "#####", "#...#", "#...#", "#...#"])
    , ('B', fromGlyph ["####.", "#...#", "#...#", "####.", "#...#", "#...#", "####."])
    , ('C', fromGlyph [".###.", "#...#", "#....", "#....", "#....", "#...#", ".###."])
    , ('D', fromGlyph ["####.", "#...#", "#...#", "#...#", "#...#", "#...#", "####."])
    , ('E', fromGlyph ["#####", "#....", "#....", "####.", "#....", "#....", "#####"])
    , ('F', fromGlyph ["#####", "#....", "#....", "####.", "#....", "#....", "#...."])
    , ('G', fromGlyph [".###.", "#...#", "#....", "#.##.", "#...#", "#...#", ".###."])
    , ('H', fromGlyph ["#...#", "#...#", "#...#", "#####", "#...#", "#...#", "#...#"])
    , ('I', fromGlyph ["#####", "..#..", "..#..", "..#..", "..#..", "..#..", "#####"])
    , ('J', fromGlyph ["..###", "...#.", "...#.", "...#.", "#..#.", "#..#.", ".##.."])
    , ('K', fromGlyph ["#...#", "#..#.", "#.#..", "##...", "#.#..", "#..#.", "#...#"])
    , ('L', fromGlyph ["#....", "#....", "#....", "#....", "#....", "#....", "#####"])
    , ('M', fromGlyph ["#...#", "##.##", "#.#.#", "#...#", "#...#", "#...#", "#...#"])
    , ('N', fromGlyph ["#...#", "##..#", "#.#.#", "#..##", "#...#", "#...#", "#...#"])
    , ('O', fromGlyph [".###.", "#...#", "#...#", "#...#", "#...#", "#...#", ".###."])
    , ('P', fromGlyph ["####.", "#...#", "#...#", "####.", "#....", "#....", "#...."])
    , ('Q', fromGlyph [".###.", "#...#", "#...#", "#...#", "#.#.#", "#..#.", ".##.#"])
    , ('R', fromGlyph ["####.", "#...#", "#...#", "####.", "#.#..", "#..#.", "#...#"])
    , ('S', fromGlyph [".###.", "#...#", "#....", ".###.", "....#", "#...#", ".###."])
    , ('T', fromGlyph ["#####", "..#..", "..#..", "..#..", "..#..", "..#..", "..#.."])
    , ('U', fromGlyph ["#...#", "#...#", "#...#", "#...#", "#...#", "#...#", ".###."])
    , ('V', fromGlyph ["#...#", "#...#", "#...#", "#...#", "#...#", ".#.#.", "..#.."])
    , ('W', fromGlyph ["#...#", "#...#", "#...#", "#.#.#", "#.#.#", "##.##", "#...#"])
    , ('X', fromGlyph ["#...#", "#...#", ".#.#.", "..#..", ".#.#.", "#...#", "#...#"])
    , ('Y', fromGlyph ["#...#", "#...#", ".#.#.", "..#..", "..#..", "..#..", "..#.."])
    , ('Z', fromGlyph ["#####", "....#", "...#.", "..#..", ".#...", "#....", "#####"])
    , ('_', fromGlyph [".....", ".....", ".....", ".....", ".....", ".....", "#####"])
    ]

fromGlyph :: [String] -> [(Int, Int)]
fromGlyph rows =
    [ (col, row)
    | (row, rowStr) <- zip ([0 ..] :: [Int]) rows
    , (col, ch) <- zip ([0 ..] :: [Int]) rowStr
    , ch == '#'
    ]

drawEdges :: [(Point, Point)] -> Picture
drawEdges = Pictures . map (\(a, b) -> Color black $ Line [a, b])

drawNodes :: [(Point, String)] -> Picture
drawNodes = Pictures . map drawNode
