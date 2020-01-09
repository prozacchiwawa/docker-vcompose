module Util.Glyph
  (
  )
where

import qualified Data.Char as Char
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

detectTopSegment :: CharPlane -> (Int,Int) -> Maybe (Int,Int)
detectTopSegment plane at =
  detectLineSegment
    plane
    (\(x,y) -> (x+1,y))
    (\ch -> ch == '-' || Char.isAlphaNum ch)
    ((==) '.')
    6
    at

detectLeftSegment :: CharPlane -> (Int,Int) -> Maybe (Int,Int)
detectLeftSegment plane at =
  detectLineSegment
    plane
    (\(x,y) -> (x,y+1))
    (\ch -> ch == '|' || Char.isAlphaNum ch)
    ((==) '`')
    2
    at

detectBottomSegment :: CharPlane -> (Int,Int) -> Maybe (Int,Int)
detectBottomSegment plane at =
  detectLineSegment
    plane
    (\(x,y) -> (x+1,y))
    (\ch -> ch == '-' || Char.isAlphaNum ch)
    ((==) '\'')
    6
    at

detectRightSegment :: CharPlane -> (Int,Int) -> Maybe (Int,Int)
detectRightSegment plane at =
  detectLineSegment
    plane
    (\(x,y) -> (x,y+1))
    (\ch -> ch == '|' || Char.isAlphaNum ch)
    ((==) '\'')
    2
    at

detectGlyph :: CharPlane -> (Int,Int) -> Maybe Rect
detectGlyph plane at@(x,y) =
  let
    topSeg = detectTopSegment plane at
    leftSeg = detectLeftSegment plane at
    botSeg = leftSeg >>= detectBottomSegment plane
    rightSeg = topSeg >>= detectRightSegment plane
  in
  case (topSeg, leftSeg, botSeg, rightSeg) of
    (Just tr, Just bl, Just br, Just brc@(ex,ey)) ->
      if br /= brc then
        Nothing
      else
        Just $ Rect x y (ex - x) (ey - y)
    _ -> Nothing
