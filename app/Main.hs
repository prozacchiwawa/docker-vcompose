module Main where

import Control.Exception
import Control.Monad
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

data FailedToAssembleException = FailedToAssembleException String deriving (Show)
instance Exception FailedToAssembleException

data ParseState
  = EmptyParseState
  | GotDoubleDash
  deriving (Show)

data ParseArguments = ParseArguments
  { parseState :: ParseState
  , parseDrawingPath :: Maybe String
  , parseVariables :: Map String String
  }
  deriving (Show)

data ParsedArguments = ParsedArguments
  { parsedDrawingPath :: String
  , parsedVariables :: Map String String
  }

parseArguments :: [String] -> Either String ParsedArguments
parseArguments args = do
  result <- foldM parseArg (ParseArguments EmptyParseState Nothing Map.empty) args
  maybe
    (Left "No drawing file specified")
    (\dpath ->
        Right $ ParsedArguments
          { parsedDrawingPath = dpath
          , parsedVariables = parseVariables result
          }
    )
    (parseDrawingPath result)

  where
    parseArg :: ParseArguments -> String -> Either String ParseArguments
    parseArg pa@ParseArguments {..} arg = do
      case parseState of
        EmptyParseState ->
          if List.isPrefixOf "-D" arg then
            parseAsVariable (drop 2 arg) pa
          else if arg == "--" then
            Right $ pa { parseState = GotDoubleDash }
          else
            takeDrawingPath arg pa

        GotDoubleDash -> takeDrawingPath arg pa

    parseAsVariable :: String -> ParseArguments -> Either String ParseArguments
    parseAsVariable arg pa =
      let
        (varname, varval) =
          (List.takeWhile ((/=) '=') arg, List.dropWhile ((/=) '=') arg)
      in
      if List.isPrefixOf "=" varval then
        Right $ pa { parseVariables = Map.insert varname (List.drop 1 varval) (parseVariables pa) }
      else
        Left $ "Command line bindings require an = (so empty ones are explicitly empty)"

    takeDrawingPath :: String -> ParseArguments -> Either String ParseArguments
    takeDrawingPath arg pa =
      if parseDrawingPath pa == Nothing then
        Right $ pa { parseDrawingPath = Just arg }
      else
        Left "Program takes one non-option argument; the path to the drawing"

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
  result <- runExceptT $ do
    ParsedArguments {..} <- ExceptT $ pure $ parseArguments args

    drawingText <- ExceptT $ Right <$> readFile parsedDrawingPath

    let
      dirOfDwg = Path.takeDirectory parsedDrawingPath
      charPlane = charPlaneFromString drawingText

    drawing :: GlyphDrawing Aeson.Value <- ExceptT $ pure $ showTopLvlExn $
      getDrawing
        (Yaml.decodeEither' . Text.encodeUtf8 . Text.pack)
        (getTopLevelBinding "id")
        charPlane

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

    sysdef <-
      ExceptT $ pure $ assembleSystem drawing system machines protocols parsedVariables

    sysyaml <- ExceptT $ pure $ createSystemYaml drawing sysdef

    pure sysyaml

  either
    (throwIO . FailedToAssembleException)
    (putStrLn . Text.unpack . Text.decodeUtf8 . Yaml.encode)
    result
