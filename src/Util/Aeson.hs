module Util.Aeson
  ( unstring
  , getTopLevelValue
  , getTopLevelBinding
  , addKey
  , emptyObject
  , identifyCheckReplaceVariables
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Text as Text

unstring :: Aeson.Value -> Maybe String
unstring (Aeson.String s) = Just $ Text.unpack s
unstring _ = Nothing

getTopLevelValue :: String -> Aeson.Value -> Maybe Aeson.Value
getTopLevelValue name (Aeson.Object o) = HashMap.lookup (Text.pack name) o
getTopLevelValue _ _ = Nothing

getTopLevelBinding :: String -> Aeson.Value -> Maybe String
getTopLevelBinding name v = unstring =<< getTopLevelValue name v

addKey :: String -> Aeson.Value -> Aeson.Value -> Aeson.Value
addKey name v (Aeson.Object o) = Aeson.Object $ HashMap.insert (Text.pack name) v o
addKey _ _ v = v

emptyObject :: Aeson.Value
emptyObject = Aeson.Object HashMap.empty

doStringReplacements :: (String -> Maybe String) -> String -> String
doStringReplacements getReplacement [] = []
doStringReplacements getReplacement ('$':'$':tl) = '$':doStringReplacements getReplacement tl
doStringReplacements getReplacement ('$':'{':tl) =
  let
    varname = takeWhile ((/=) '}') tl
    restOfString = List.drop 1 $ List.dropWhile ((/=) '}') tl
  in
  maybe
    ('$':'{':doStringReplacements getReplacement tl)
    ((++) $ doStringReplacements getReplacement restOfString)
    (getReplacement varname)
doStringReplacements getReplacement (hd:tl) = hd:doStringReplacements getReplacement tl

identifyCheckReplaceVariables :: (String -> Maybe String) -> Aeson.Value -> Aeson.Value
identifyCheckReplaceVariables getReplacement (Aeson.String t) =
  let
    s = Text.unpack t
    replaced = doStringReplacements getReplacement s
  in
  Aeson.String $ Text.pack replaced

identifyCheckReplaceVariables getReplacement (Aeson.Object o) =
  Aeson.Object $ (identifyCheckReplaceVariables getReplacement) <$> o
identifyCheckReplaceVariables getReplacement (Aeson.Array a) =
  Aeson.Array $ (identifyCheckReplaceVariables getReplacement) <$> a
identifyCheckReplaceVariables getReplacement v = v

