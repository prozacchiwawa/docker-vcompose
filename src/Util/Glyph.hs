module Util.Glyph
  ( GlyphId (..)
  , GlyphContent (..)
  , GlyphVertex (..)
  , GlyphDrawing (..)
  , commas
  , detectGlyph
  , detectGlyphs
  , getGlyphText
  , getGlyphPorts
  , findPath
  , getDrawing
  )
where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Set (Set)
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
  { gBindings :: Map String String
  , gPorts :: Map Char (Int,Int)
  , gRect :: Rect
  }
  deriving (Show, Eq, Ord)

data GlyphId
  = GlyphId String
  | GlyphRect Rect
  deriving (Show, Eq, Ord)

data GlyphVertex = GlyphVertex
  { toGlyph :: GlyphId
  , conn :: Char
  }
  deriving (Show, Eq, Ord)

data GlyphDrawing = GlyphDrawing
  { glyphs :: Map GlyphId GlyphContent
  , nets :: Map GlyphVertex (Set GlyphVertex)
  }
  deriving (Show, Eq, Ord)

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

coordsOnRect :: Rect -> [(Int,Int)]
coordsOnRect Rect {..} =
  topCoords x w ++ leftCoords y h ++ rightCoords y h ++ botCoords x w
  where
    topCoords j l = if l == 0 then [] else (j,y):topCoords (j+1) (l-1)
    botCoords j l = if l == 0 then [] else (j,y+h):botCoords (j+1) (l-1)
    leftCoords i l = if l == 0 then [] else (x,i):leftCoords (i+1) (l-1)
    rightCoords i l = if l == 0 then [] else (x+w,i):rightCoords (i+1) (l-1)

getGlyphPorts :: CharPlane -> Rect -> Map Char (Int,Int)
getGlyphPorts plane Rect {..} =
  let
    rectPoints = coordsOnRect (Rect x y (w-1) (h-1))
    pairs = (\(j,i) -> (getCharAt plane j i, (j,i))) <$> rectPoints
    onlySymbols = filter (Char.isAlphaNum . fst) pairs
  in
  Map.fromList onlySymbols

colonSplit :: String -> (String,String)
colonSplit str =
  let
    name = trim $ List.takeWhile ((/=) ':') str
    val = trim $ dropWhile ((==) ':') $ dropWhile ((/=) ':') str
  in
  (name, val)

glyphTextToBindings :: String -> Map String String
glyphTextToBindings str =
  Map.fromList $ colonSplit <$> lines str

-- | Given a char plane, find a path from c1 to c2 through |, -, + and @ chars.
-- The path must change direction in a + char, and must not change direction in an @ char.
findPath :: CharPlane -> Set (Int,Int) -> (Int,Int) -> (Int,Int) -> Bool
findPath plane rectpts pt other =
  runOne Set.empty $ Set.singleton pt
  where
    runOne :: Set (Int,Int) -> Set (Int,Int) -> Bool
    runOne visited forefront =
      let
        nextStep = advance visited forefront
      in
      if null nextStep then -- No remaining steps to be taken
        False
      else
        if Set.member other nextStep then
          True
        else
          runOne (Set.union visited forefront) nextStep

    advance :: Set (Int,Int) -> Set (Int,Int) -> Set (Int,Int)
    advance visited forefront =
      Set.difference
        (Set.unions $ validNeighbors <$> Set.toList forefront)
        (Set.unions $ [visited, forefront, rectpts])

    validNeighbors :: (Int,Int) -> Set (Int,Int)
    validNeighbors (x,y) =
      let
        leftN = nextList '-' (\(j,i) -> (j-1,i))
        rightN = nextList '-' (\(j,i) -> (j+1,i))
        topN = nextList '|' (\(j,i) -> (j,i-1))
        botN = nextList '|' (\(j,i) -> (j,i+1))

        nextList want nextStep =
          let
            toward@(tx,ty) = nextStep (x,y)
            thatChar = getCharAt plane tx ty
            crawl =
              if thatChar == want then
                Set.singleton toward
              else if thatChar == '@' then
                Set.filter ((/=) (x,y)) $ validNeighbors (nextStep toward)
              else if thatChar == '+' then
                Set.filter ((/=) (x,y)) $ validNeighbors toward
              else
                Set.empty
          in
          if toward == other then
            Set.singleton toward
          else
            crawl

      in
      Set.unions [leftN, rightN, topN, botN]

createNets
  :: CharPlane
  -> [((Int,Int), GlyphContent, GlyphVertex)]
  -> Map GlyphVertex (Set GlyphVertex)
createNets plane inputs =
  List.foldl'
    (\vmap inp@(pt,GlyphContent {..},vt) ->
       let
         insetRect = Rect (x gRect) (y gRect) ((w gRect) - 1) ((h gRect) - 1)
         inotme = filter ((/=) inp) inputs
         matches =
           filter
             (\(otherpt,_,_) ->
                findPath plane (Set.fromList $ coordsOnRect insetRect) pt otherpt
             )
             inotme
       in
       if not $ null matches then
         Map.insert vt (Set.fromList $ (\(_,_,v) -> v) <$> matches) vmap
       else
         vmap
    )
    Map.empty
    inputs

getDrawing :: CharPlane -> GlyphDrawing
getDrawing plane =
  let
    cs = commas plane
    glyphRects = detectGlyphs plane cs

    rawGlyphs =
      (\r ->
          (r, glyphTextToBindings $ getGlyphText plane r, getGlyphPorts plane r)
      ) <$> glyphRects

    labeledGlyphs :: [(GlyphId, GlyphContent)] =
      (\(rect, bindings, ports) ->
          let
            gid =
              maybe
                (GlyphRect rect)
                GlyphId
                (Map.lookup "id" bindings)
          in
          (gid, GlyphContent bindings ports rect)
      ) <$> rawGlyphs

    glyphPorts :: [((Int,Int), GlyphContent, GlyphVertex)] =
      concat $
        (\(gid, gc@GlyphContent {..}) ->
           let
             portPts = Map.toList gPorts
           in
           (\(ch,pt) -> (pt,gc,GlyphVertex gid ch)) <$> portPts
        ) <$> labeledGlyphs

    glyphNets = createNets plane glyphPorts
  in
  GlyphDrawing
    { glyphs = Map.fromList labeledGlyphs
    , nets = glyphNets
    }
