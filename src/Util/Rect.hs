module Util.Rect
  ( Rect (..)
  )
where

data Rect =
  Rect
    { x :: Int
    , y :: Int
    , w :: Int
    , h :: Int
    }
