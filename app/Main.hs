module Main where

import Control.Monad.Trans.Except

import qualified Data.Aeson as Aeson
import Data.Either
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Yaml as Yaml
import qualified System.Directory as Dir
import qualified System.Environment as Sys
import qualified System.FilePath as Path
import qualified System.IO as Sys

import Docker.Drawing
import Docker.System
import Util.CharPlane
import Util.Glyph

getSourceFileNames :: String -> [String] -> IO [String]
getSourceFileNames ext dirs =
  concat <$> traverse yamlsFromDir dirs
  where
    yamlsFromDir dir = do
      entries :: [String] <- Dir.listDirectory dir

      pure $ filter (Path.isExtensionOf ext) $ (Path.combine dir) <$> entries

readAndParseYaml :: (Aeson.FromJSON a) => String -> IO (Either String a)
readAndParseYaml file = do
  fileBody <- readFile file
  pure $ parseYaml fileBody

parseFilesAs :: (Aeson.FromJSON a) => [String] -> IO (Either String [(String,a)])
parseFilesAs files = do
  results <-
    traverse
      (\f -> do
         parsed <- readAndParseYaml f
         pure $ either (Left . (f,)) (Right . (f,)) parsed
      )
      files
  let
    (errors, parsed) = partitionEithers results

  pure $
    if null errors then
      Right parsed
    else
      Left (List.intercalate "\n" $ show <$> errors)

includeMachineTemplate
  :: (String,MachineDefYaml)
  -> IO (String,MachineDefYaml)
includeMachineTemplate (filepath,mdy@MachineDefYaml {..}) = do
  let
    targetPath = Path.combine (Path.takeDirectory filepath) mdyBaseYaml
  templateContent <- readFile targetPath
  pure $ (filepath, mdy { mdyBaseYaml = templateContent })

includeMachineTemplates :: [(String,MachineDefYaml)] -> IO [(String,MachineDefYaml)]
includeMachineTemplates = traverse includeMachineTemplate

showTopLvlExn :: Show e => Either [e] v -> Either String v
showTopLvlExn = either (Left . List.intercalate "," . (fmap show)) Right

main :: IO ()
main = do
  args <- Sys.getArgs
  case args of
    [dwg] -> do
      drawingText <- readFile dwg
      let
        charPlane = charPlaneFromString drawingText

      result <- runExceptT $ do
        drawing :: GlyphDrawing Aeson.Value <- ExceptT $ pure $ showTopLvlExn $
          getDrawing
            (Yaml.decodeEither' . Text.encodeUtf8 . Text.pack)
            (getTopLevelBinding "id")
            charPlane

        let
          dirOfDwg = Path.takeDirectory dwg

        sysYamlPath <- ExceptT $ pure $ getSystemYamlFromDrawing drawing
        let
          sysYamlFullPath = Path.combine dirOfDwg sysYamlPath
          sysYamlParentDir = Path.takeDirectory sysYamlFullPath

        sysYaml <- ExceptT $ Right <$> readFile sysYamlFullPath
        system :: DockerSystemYaml <- ExceptT $ pure $ parseYaml sysYaml

        let
          sourceFullPaths = Path.combine sysYamlParentDir <$> dsySourceDirs system

        protosList <- ExceptT $ Right <$> getSourceFileNames "netproto" sourceFullPaths
        machinesList <- ExceptT $ Right <$> getSourceFileNames "machine" sourceFullPaths

        protocolDefsRaw :: [(String,[NetProtoYaml])] <- ExceptT $ parseFilesAs protosList
        let
          protocolDefs =
            Map.fromList $
              (\np@NetProtoYaml {..} -> (npyName,np)) <$> concat (snd <$> protocolDefsRaw)

        protocols <- ExceptT $ pure $ realizeProtocolDefs protocolDefs

        machineDefsRaw :: [(String,MachineDefYaml)] <- ExceptT $ parseFilesAs machinesList
        machineDefsTmplRaw <- ExceptT $ Right <$> includeMachineTemplates machineDefsRaw
        let
          machineDefs =
            Map.fromList $
              (\(n,v) -> (Path.dropExtension $ Path.takeFileName n, v)) <$> machineDefsTmplRaw

        machines <- ExceptT $ pure $ realizeMachineDefs protocols machineDefs

        pure (protocols, machineDefs, drawing, system)

      putStrLn $ show result
    _ -> do
      putStrLn "Usage: vcompose [drawing]"
