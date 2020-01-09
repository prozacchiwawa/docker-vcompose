module Util.CharPlane
  ( CharPlane (..)
  , getCharAt
  )
where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Vector (Vector, (!?))
import qualified Data.Vector as Vector

data CharPlane = CharPlane
  { maxX :: Int
  , rows :: Vector (Vector Char)
  }

charPlaneFromString :: String -> CharPlane
charPlaneFromString str =
  let
    rows = List.lines str
    maxx = List.foldl' max 0 $ List.length <$> rows
  in
  CharPlane
    { maxX = maxx
    , rows = Vector.fromList $ Vector.fromList <$> rows
    }

getCharAt :: CharPlane -> Int -> Int -> Char
getCharAt CharPlane {..} x y = Maybe.fromMaybe ' ' $ ((flip (Vector.!?)) x) =<< rows !? y
