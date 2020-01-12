module Docker.Drawing
  ( getSystemYamlFromDrawing
  , populateMachinesFromDrawing
  , getTopLevelBinding
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import qualified Data.Text as Text

import Docker.System as System
import Util.Aeson
import Util.Glyph

data MachineInstance = MachineInstance
  { glyphId :: GlyphId
  , glyphContent :: GlyphContent Aeson.Value
  , machineDef :: System.MachineDef
  }
  deriving (Show)

getTopLevelBinding :: String -> Aeson.Value -> Maybe String
getTopLevelBinding name (Aeson.Object o) = unstring =<< HashMap.lookup (Text.pack name) o
getTopLevelBinding _ _ = Nothing

getSystemFilenameFromGlyphContent :: GlyphContent Aeson.Value -> Maybe String
getSystemFilenameFromGlyphContent = getTopLevelBinding "system-yaml" . gData

-- | Isolate the intended system from a glyph drawing.
getDockerSystemDef :: GlyphDrawing Aeson.Value -> Either String String
getDockerSystemDef GlyphDrawing {..} =
  let
    yamls =
      catMaybes $
        (\(gid,gc) -> (gid,) <$> getSystemFilenameFromGlyphContent gc) <$> Map.toList glyphs
  in
  case yamls of
    [] -> Left "No system-yaml declaration found in any glyph"
    [(_,sy)] -> Right sy
    lst -> Left $ "Multiple system-yaml declarations found: " ++ (show $ fst <$> lst)

-- | Mutate a docker system with the contents of the drawing.
getSystemYamlFromDrawing :: GlyphDrawing Aeson.Value -> Either String String
getSystemYamlFromDrawing drawing = getDockerSystemDef drawing

populateMachinesFromDrawing
  :: GlyphDrawing Aeson.Value
  -> Map String MachineDef
  -> [MachineInstance]
populateMachinesFromDrawing = undefined
