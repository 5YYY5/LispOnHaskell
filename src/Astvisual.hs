module Astvisual (visualizeAST) where

import Data.Char (toUpper)
import Graphics.Gloss
import Types (ExprTree(..), SExpr(..), Token(..), TokenKind(..))

nodeRadius :: Float
nodeRadius   = 28

nodeDiameter :: Float
nodeDiameter = 2 * nodeRadius

horizontalGap :: Float
horizontalGap = 30

verticalGap :: Float
verticalGap   = 80

charPitch :: Float
charPitch = 2.0

strokeWidth :: Float
strokeWidth = 1.4

glyphHeight :: Int
glyphHeight = 6

glyphCols :: Int
glyphCols = 5

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
        , Color black $ drawStrokeLabel (trimLabel label)
        ]

charWidth :: Float
charWidth = fromIntegral glyphCols * charPitch

glyphTopY :: Int -> Float
glyphTopY row = fromIntegral (glyphHeight - row) * charPitch

glyphBottomY :: Int -> Float
glyphBottomY row = fromIntegral (glyphHeight - row - 1) * charPitch

drawStrokeLabel :: String -> Picture
drawStrokeLabel str =
    let totalW = fromIntegral (length str) * charWidth
        centerY = fromIntegral glyphHeight * charPitch / 2
    in Translate (-totalW / 2) (-centerY) $
       pictures
           [ Translate (fromIntegral i * charWidth) 0 (drawStrokeChar c)
           | (i, c) <- zip ([0 ..] :: [Int]) str
           ]

drawStrokeChar :: Char -> Picture
drawStrokeChar c =
    pictures
        [ thickSegment strokeWidth (p1, p2)
        | (p1, p2) <- glyphStrokes c
        ]

thickSegment :: Float -> (Point, Point) -> Picture
thickSegment w ((x1, y1), (x2, y2)) =
    let dx = x2 - x1
        dy = y2 - y1
        len = sqrt (dx * dx + dy * dy)
        (nx, ny)
            | len == 0  = (0, 0)
            | otherwise = ((-dy / len) * w * 0.45, (dx / len) * w * 0.45)
    in pictures
        [ Line [(x1 + nx * o, y1 + ny * o), (x2 + nx * o, y2 + ny * o)]
        | o <- [-1, 0, 1 :: Float]
        ]

glyphStrokes :: Char -> [((Float, Float), (Float, Float))]
glyphStrokes c = glyphToStrokes (lookupGlyphRows c)

lookupGlyphRows :: Char -> [String]
lookupGlyphRows ch = maybe (lookupGlyphRows '?') id (lookup (toUpper ch) glyphTable)

filledAt :: [String] -> Int -> Int -> Bool
filledAt _ r c | r < 0 || c < 0 = False
filledAt rows r c
    | r >= length rows = False
    | otherwise =
        let rowStr = rows !! r
        in c < length rowStr && rowStr !! c == '#'

glyphToStrokes :: [String] -> [((Float, Float), (Float, Float))]
glyphToStrokes rows =
    horizontalStrokes rows ++ verticalStrokes rows

horizontalStrokes :: [String] -> [((Float, Float), (Float, Float))]
horizontalStrokes rows =
    [ ( (x1, y), (x2, y) )
    | r <- [0 .. glyphHeight + 1]
    , c <- [0 .. glyphCols - 1]
    , filledAt rows (r - 1) c /= filledAt rows r c
    , let y  = glyphTopY r
          x1 = fromIntegral c * charPitch
          x2 = fromIntegral (c + 1) * charPitch
    ]

verticalStrokes :: [String] -> [((Float, Float), (Float, Float))]
verticalStrokes rows =
    [ ( (x, y1), (x, y2) )
    | r <- [0 .. glyphHeight]
    , c <- [0 .. glyphCols]
    , filledAt rows r (c - 1) /= filledAt rows r c
    , let x  = fromIntegral c * charPitch
          y1 = glyphTopY r
          y2 = glyphBottomY r
    ]

glyphTable :: [(Char, [String])]
glyphTable =
    [ (' ', replicate 7 ".....")
    , ('?', [ "..#..", "...#.", "..#..", ".#...", "..#..", ".....", "..#.." ])
    , ('0', [ ".###.", "#...#", "#...#", "#...#", "#...#", "#...#", ".###." ])
    , ('1', [ "..#..", ".##..", "..#..", "..#..", "..#..", "..#..", ".###." ])
    , ('2', [ ".###.", "#...#", "....#", "...#.", "..#..", ".#...", "#####" ])
    , ('3', [ ".###.", "#...#", "....#", "..##.", "....#", "#...#", ".###." ])
    , ('4', [ "#...#", "#...#", "#...#", "#####", "....#", "....#", "....#" ])
    , ('5', [ "#####", "#....", "#....", "####.", "....#", "#...#", ".###." ])
    , ('6', [ ".###.", "#...#", "#....", "####.", "#...#", "#...#", ".###." ])
    , ('7', [ "#####", "....#", "...#.", "..#..", ".#...", "#....", "#...." ])
    , ('8', [ ".###.", "#...#", "#...#", ".###.", "#...#", "#...#", ".###." ])
    , ('9', [ ".###.", "#...#", "#...#", ".####", "....#", "#...#", ".###." ])
    , ('(', [ "..#..", ".#...", "#....", "#....", "#....", ".#...", "..#.." ])
    , (')', [ "..#..", "...#.", "....#", "....#", "....#", "...#.", "..#.." ])
    , ('+', [ ".....", "..#..", "..#..", "#####", "..#..", "..#..", "....." ])
    , ('-', [ ".....", ".....", ".....", "#####", ".....", ".....", "....." ])
    , ('*', [ ".....", "#...#", ".#.#.", "..#..", ".#.#.", "#...#", "....." ])
    , ('/', [ "....#", "....#", "...#.", "..#..", ".#...", "#....", "#...." ])
    , ('=', [ ".....", ".....", "#####", ".....", "#####", ".....", "....." ])
    , ('<', [ "...#.", "..#..", ".#...", "#....", ".#...", "..#..", "...#." ])
    , ('>', [ ".#...", "..#..", "...#.", "....#", "...#.", "..#..", ".#..." ])
    , ('.', [ ".....", ".....", ".....", ".....", ".....", "..#..", "..#.." ])
    , ('\'', [ "..#..", "..#..", ".#...", ".....", ".....", ".....", "....." ])
    , ('"', [ ".#.#.", ".#.#.", ".#.#.", ".....", ".....", ".....", "....." ])
    , ('A', [ ".###.", "#...#", "#...#", "#####", "#...#", "#...#", "#...#" ])
    , ('B', [ "####.", "#...#", "#...#", "####.", "#...#", "#...#", "####." ])
    , ('C', [ ".###.", "#...#", "#....", "#....", "#....", "#...#", ".###." ])
    , ('D', [ "####.", "#...#", "#...#", "#...#", "#...#", "#...#", "####." ])
    , ('E', [ "#####", "#....", "#....", "####.", "#....", "#....", "#####" ])
    , ('F', [ "#####", "#....", "#....", "####.", "#....", "#....", "#...." ])
    , ('G', [ ".###.", "#...#", "#....", "#.##.", "#...#", "#...#", ".###." ])
    , ('H', [ "#...#", "#...#", "#...#", "#####", "#...#", "#...#", "#...#" ])
    , ('I', [ "#####", "..#..", "..#..", "..#..", "..#..", "..#..", "#####" ])
    , ('J', [ "..###", "...#.", "...#.", "...#.", "#..#.", "#..#.", ".##.." ])
    , ('K', [ "#...#", "#..#.", "#.#..", "##...", "#.#..", "#..#.", "#...#" ])
    , ('L', [ "#....", "#....", "#....", "#....", "#....", "#....", "#####" ])
    , ('M', [ "#...#", "##.##", "#.#.#", "#...#", "#...#", "#...#", "#...#" ])
    , ('N', [ "#...#", "##..#", "#.#.#", "#..##", "#...#", "#...#", "#...#" ])
    , ('O', [ ".###.", "#...#", "#...#", "#...#", "#...#", "#...#", ".###." ])
    , ('P', [ "####.", "#...#", "#...#", "####.", "#....", "#....", "#...." ])
    , ('Q', [ ".###.", "#...#", "#...#", "#...#", "#.#.#", "#..#.", ".##.#" ])
    , ('R', [ "####.", "#...#", "#...#", "####.", "#.#..", "#..#.", "#...#" ])
    , ('S', [ ".###.", "#...#", "#....", ".###.", "....#", "#...#", ".###." ])
    , ('T', [ "#####", "..#..", "..#..", "..#..", "..#..", "..#..", "..#.." ])
    , ('U', [ "#...#", "#...#", "#...#", "#...#", "#...#", "#...#", ".###." ])
    , ('V', [ "#...#", "#...#", "#...#", "#...#", "#...#", ".#.#.", "..#.." ])
    , ('W', [ "#...#", "#...#", "#...#", "#.#.#", "#.#.#", "##.##", "#...#" ])
    , ('X', [ "#...#", "#...#", ".#.#.", "..#..", ".#.#.", "#...#", "#...#" ])
    , ('Y', [ "#...#", "#...#", ".#.#.", "..#..", "..#..", "..#..", "..#.." ])
    , ('Z', [ "#####", "....#", "...#.", "..#..", ".#...", "#....", "#####" ])
    , ('_', [ ".....", ".....", ".....", ".....", ".....", ".....", "#####" ])
    ]

drawEdges :: [(Point, Point)] -> Picture
drawEdges = Pictures . map (\(a, b) -> Color black $ Line [a, b])

drawNodes :: [(Point, String)] -> Picture
drawNodes = Pictures . map drawNode
