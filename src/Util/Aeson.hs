module Util.Aeson
  ( unstring
  , getTopLevelValue
  , getTopLevelBinding
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

unstring :: Aeson.Value -> Maybe String
unstring (Aeson.String s) = Just $ Text.unpack s
unstring _ = Nothing

getTopLevelValue :: String -> Aeson.Value -> Maybe Aeson.Value
getTopLevelValue name (Aeson.Object o) = HashMap.lookup (Text.pack name) o
getTopLevelValue _ _ = Nothing

getTopLevelBinding :: String -> Aeson.Value -> Maybe String
getTopLevelBinding name v = unstring =<< getTopLevelValue name v
