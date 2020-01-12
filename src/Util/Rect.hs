module Util.Rect
  ( Rect (..)
  , coordInRect
  )
where

import GHC.Generics

import qualified Data.Aeson as Aeson

data Rect =
  Rect
    { x :: Int
    , y :: Int
    , w :: Int
    , h :: Int
    }
    deriving (Show, Eq, Ord, Generic)

instance Aeson.ToJSON Rect

coordInRect :: Rect -> Int -> Int -> Bool
coordInRect Rect {..} j i = j >= x && j < x + w && i >= y && i < y + h
