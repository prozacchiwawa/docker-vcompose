module Util.Aeson
  ( unstring
  , getTopLevelValue
  , getTopLevelBinding
  , addKey
  , keysOfDict
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

doStringReplacements :: (String -> Either String String) -> String -> Either String String
doStringReplacements getReplacement [] = Right []
doStringReplacements getReplacement ('$':'$':tl) =
  ('$':) <$> doStringReplacements getReplacement tl
doStringReplacements getReplacement ('$':'{':tl) = do
  let
    varname = takeWhile ((/=) '}') tl
    restOfString = List.drop 1 $ List.dropWhile ((/=) '}') tl

  newPrefix <- getReplacement varname
  newSuffix <- doStringReplacements getReplacement restOfString
  pure $ newPrefix ++ newSuffix

doStringReplacements getReplacement (hd:tl) =
  (hd:) <$> doStringReplacements getReplacement tl

identifyCheckReplaceVariables
  :: (String -> Either String String) -> Aeson.Value -> Either String Aeson.Value
identifyCheckReplaceVariables getReplacement (Aeson.String t) =
  let
    s = Text.unpack t
    replaced = doStringReplacements getReplacement s
  in
  (Aeson.String . Text.pack) <$> replaced

identifyCheckReplaceVariables getReplacement (Aeson.Object o) = do
  replacementValues <- traverse (identifyCheckReplaceVariables getReplacement) o
  pure $ Aeson.Object replacementValues
identifyCheckReplaceVariables getReplacement (Aeson.Array a) = do
  replacementValues <- traverse (identifyCheckReplaceVariables getReplacement) a
  pure $ Aeson.Array replacementValues
identifyCheckReplaceVariables getReplacement v = pure v

keysOfDict :: Aeson.Value -> Either String [String]
keysOfDict (Aeson.Object o) = Right $ Text.unpack <$> HashMap.keys o
keysOfDict _ = Left "No keys in non-object"
