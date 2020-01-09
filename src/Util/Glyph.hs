module Util.Glyph
  ( commas
  , detectGlyph
  , detectGlyphs
  , getGlyphText
  )
where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Maybe as Maybe
import qualified Data.Vector as Vector
import Data.Vector (Vector)

import Util.Rect
import Util.CharPlane

{-

Make glyphs like this:

  comma                  dot
  |                      |
  v                      v
  ,----------------------.
  |                      |
  |  Data: value         |  <-- glyphs contain text.
  |  Key: val            |
  |                      Q  <-- letters or numbers embedded in the frame have meaning.
  `------I-----M---------'
  ^
  |
  backtick

-}

data GlyphContent = GlyphContent
  { bindings :: Map String String
  , ports :: Map Char (Int,Int)
  }

-- | Find outside box commas that could be the upper left corners of glyphs.
commas :: CharPlane -> [(Int,Int)]
commas plane@CharPlane {..} =
  let
    xcoords :: [Int] = [0..(maxX - 1)]
    coords :: [(Int,Int)] =
      concat $ (\y -> (,y) <$> xcoords) <$> [0..(Vector.length rows - 1)]
  in
  filter (\(x,y) -> ',' == getCharAt plane x y) coords

-- | Measure the top or left line segments of a glyph starting at the given coordinate
-- in the char plane.  Return Nothing if there's not enough sensible geometry.
detectLineSegment
  :: CharPlane
  -> ((Int,Int) -> (Int,Int))
  -> (Char -> Bool)
  -> (Char -> Bool)
  -> Int
  -> (Int,Int)
  -> Maybe (Int,Int)
detectLineSegment plane nextCoord validChar isEndChar minLength at =
  let
    coord@(x,y) = nextCoord at
    charThere = getCharAt plane x y
  in
  if validChar charThere then
    detectLineSegment plane nextCoord validChar isEndChar (minLength-1) coord
  else if minLength < 1 && isEndChar charThere then
    Just coord
  else
    Nothing

minWidth :: Int
minWidth = 5

minHeight :: Int
minHeight = 1

detectTopSegment :: CharPlane -> (Int,Int) -> Maybe (Int,Int)
detectTopSegment plane at =
  detectLineSegment
    plane
    (\(x,y) -> (x+1,y))
    (\ch -> ch == '-' || Char.isAlphaNum ch)
    ((==) '.')
    minWidth
    at

detectLeftSegment :: CharPlane -> (Int,Int) -> Maybe (Int,Int)
detectLeftSegment plane at =
  detectLineSegment
    plane
    (\(x,y) -> (x,y+1))
    (\ch -> ch == '|' || Char.isAlphaNum ch)
    ((==) '`')
    minHeight
    at

detectBottomSegment :: CharPlane -> (Int,Int) -> Maybe (Int,Int)
detectBottomSegment plane at =
  detectLineSegment
    plane
    (\(x,y) -> (x+1,y))
    (\ch -> ch == '-' || Char.isAlphaNum ch)
    ((==) '\'')
    minWidth
    at

detectRightSegment :: CharPlane -> (Int,Int) -> Maybe (Int,Int)
detectRightSegment plane at =
  detectLineSegment
    plane
    (\(x,y) -> (x,y+1))
    (\ch -> ch == '|' || Char.isAlphaNum ch)
    ((==) '\'')
    minHeight
    at

detectGlyph :: CharPlane -> (Int,Int) -> Maybe Rect
detectGlyph plane at@(x,y) =
  let
    topSeg = detectTopSegment plane at
    leftSeg = detectLeftSegment plane at
    botSeg = leftSeg >>= detectBottomSegment plane
    rightSeg = topSeg >>= detectRightSegment plane
    matchTup = (topSeg, leftSeg, botSeg, rightSeg)
  in
  case matchTup of
    (Just tr, Just bl, Just br, Just brc@(ex,ey)) ->
      if br /= brc then
        Nothing
      else
        Just $ Rect x y (ex - x + 1) (ey - y + 1)
    _ -> Nothing

-- | Detect whether each location represents a glyph and ensure that subsequent coords within
-- the glyph are not tried.
detectGlyphs :: CharPlane -> [(Int,Int)] -> [Rect]
detectGlyphs _ [] = []
detectGlyphs plane (coord:coords) =
  let
    detected = detectGlyph plane coord
    following = Maybe.maybe id (\r -> filter (\(x,y) -> not $ coordInRect r x y)) detected
  in
  Maybe.maybeToList detected ++ detectGlyphs plane (following coords)

trim :: String -> String
trim str =
  let
    tb = dropWhile Char.isSpace str
  in
  reverse $ dropWhile Char.isSpace $ reverse tb

-- | Get glyph text
-- Returns a little string formatted as though it was a standalone text file containing the
-- trimmed contents of the rectangle.
getGlyphText :: CharPlane -> Rect -> String
getGlyphText plane Rect {..} =
  let
    rowNums = [y+1..(y+h)-2]
    colNums = [x+1..(x+w)-2]
    rawRows = (\i -> (\j -> getCharAt plane j i) <$> colNums) <$> rowNums
    rows = filter ((/=) "") $ trim <$> rawRows
  in
  List.intercalate "\n" rows
