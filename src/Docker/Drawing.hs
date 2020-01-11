module Docker.Drawing
  ( getSystemYamlFromDrawing
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe

import Docker.System as System
import Util.Aeson
import Util.Glyph

getSystemFilenameFromGlyphContent :: GlyphContent -> Maybe String
getSystemFilenameFromGlyphContent GlyphContent {..} =
  Map.lookup "system-yaml" gBindings

-- | Isolate the intended system from a glyph drawing.
getDockerSystemDef :: GlyphDrawing -> Either String String
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
getSystemYamlFromDrawing :: GlyphDrawing -> Either String String
getSystemYamlFromDrawing drawing =
  getDockerSystemDef drawing
