module Main where

import Control.Monad.Trans.Except

import qualified Data.Aeson as Aeson
import Data.Either
import qualified Data.List as List
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

parseFilesAs :: (Aeson.FromJSON a) => [String] -> IO (Either String [a])
parseFilesAs files = do
  results <-
    traverse
      (\f -> do
         parsed <- readAndParseYaml f
         pure $ either (Left . (f,)) Right parsed
      )
      files
  let
    (errors, parsed) = partitionEithers results

  pure $
    if null errors then
      Right parsed
    else
      Left (List.intercalate "\n" $ show <$> errors)

main :: IO ()
main = do
  args <- Sys.getArgs
  case args of
    [dwg] -> do
      drawingText <- readFile dwg
      let
        charPlane = charPlaneFromString drawingText
        drawing = getDrawing charPlane
        dirOfDwg = Path.takeDirectory dwg

      result <- runExceptT $ do
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

        protocols :: [[NetProtoYaml]] <- ExceptT $ parseFilesAs protosList
        machines :: [MachineDefYaml] <- ExceptT $ parseFilesAs machinesList

        pure (protocols, machines, system)

      putStrLn $ show result
    _ -> do
      putStrLn "Usage: vcompose [drawing]"
