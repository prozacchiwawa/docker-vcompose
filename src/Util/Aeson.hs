module Util.Aeson
  ( unstring
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

unstring :: Aeson.Value -> Maybe String
unstring (Aeson.String s) = Just $ Text.unpack s
unstring _ = Nothing
