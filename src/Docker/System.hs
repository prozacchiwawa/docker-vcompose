module Docker.System
  ( DockerSystem (..)
  , DockerSystemYaml (..)
  , NetProtoYaml (..)
  , MachineDefYaml (..)
  , MachineDef (..)
  , parseYaml
  , realizeProtocolDefs
  , realizeMachineDefs
  )
where

import GHC.Generics

import qualified Data.Aeson as Aeson
import Data.Either
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Yaml as Yaml

import Util.Glyph

data NetProto = NetProto
  { npName :: String
  , npSpecializes :: Set NetProto
  }
  deriving (Show, Eq, Ord)

data MachineDefListenPort = MachineDefListenPort
  { mdpName :: String
  , mdpChar :: Char
  , mdpProtocol :: NetProto
  , mdpInternal :: Int
  , mdpExternal :: Maybe Int
  }
  deriving (Show, Eq, Ord)

data MachineDefMount = MachineDefMount
  { mdmName :: String
  , mdmChar :: Char
  , mdmTargetPath :: String
  }
  deriving (Show, Eq, Ord)

data MachineDefConn = MachineDefConn
  { mdcName :: String
  , mdcChar :: Char
  , mdcProtocol :: NetProto
  }
  deriving (Show, Eq, Ord)

data MachineDef = MachineDef
  { mdName :: String
  , mdTemplate :: String
  , mdPorts :: Map Char MachineDefListenPort
  , mdConnections :: Map Char MachineDefConn
  , mdMounts :: Map Char MachineDefMount
  }
  deriving (Show, Eq, Ord)

data DockerSystem = DockerSystem
  {
{-
    dsBaseYaml :: Aeson.Value
  , dsMachineDefs :: Map String MachineDef
  , dsProtocols :: Set NetProto
  , dsLocalDirs :: [String]
  , dsDrawing :: GlyphDrawing
-}
  }
  deriving (Show)

data NetProtoYaml = NetProtoYaml
  { npyName :: String
  , npySpecializes :: [String]
  }
  deriving (Show, Generic)

instance Aeson.FromJSON NetProtoYaml

data NetListenPortYaml = NetListenPortYaml
  { nlpyName :: String
  , nlpyLabel :: String
  , nlpyProtocol :: String
  , nlpyPort :: Int
  , nlpyDefaultHostPort :: Maybe Int
  }
  deriving (Show, Generic)

instance Aeson.FromJSON NetListenPortYaml

data NetConnectPortYaml = NetConnectPortYaml
  { ncpyName :: String
  , ncpyLabel :: String
  , ncpyProtocol :: String
  }
  deriving (Show, Generic)

instance Aeson.FromJSON NetConnectPortYaml

data MachineDefMountYaml = MachineDefMountYaml
  { mdymName :: String
  , mdymLabel :: String
  , mdymTarget :: String
  }
  deriving (Show, Generic)

instance Aeson.FromJSON MachineDefMountYaml

data MachineDefYaml = MachineDefYaml
  { mdyBaseYaml :: String
  , mdyListenPorts :: [NetListenPortYaml]
  , mdyConnectPorts :: [NetConnectPortYaml]
  , mdyMounts :: [MachineDefMountYaml]
  }
  deriving (Show, Generic)

instance Aeson.FromJSON MachineDefYaml

data DockerSystemYaml = DockerSystemYaml
  { dsyBaseYaml :: String
  , dsySourceDirs :: [String]
  }
  deriving (Show, Generic)

instance Aeson.FromJSON DockerSystemYaml

realizeProtocolDef :: Set String -> Map String NetProtoYaml -> String -> Either String NetProto
realizeProtocolDef path defs proto =
  if Set.member proto path then
    Left $ "Protocol cycle detected: " ++ show path ++ " precede " ++ proto
  else
    maybe
      (Left $ "No protocol named " ++ proto ++ " was read")
      (\npy@NetProtoYaml {..} ->
         let
           newPath = Set.insert proto path
           (errors, parents) =
             partitionEithers $ realizeProtocolDef newPath defs <$> npySpecializes
         in
         if null errors then
           Right $ NetProto npyName $ Set.fromList parents
         else
           Left $ List.intercalate "," $ show <$> errors
      )
      (Map.lookup proto defs)

realizeMachineDef
  :: Map String NetProto
  -> Map String MachineDefYaml
  -> String
  -> Either String MachineDef
realizeMachineDef protos defs machname =
  maybe
    (Left $ "No machine named " ++ machname ++ " found")
    yamlToRealMachine
    (Map.lookup machname defs)
  where
    realizeListenPort :: NetListenPortYaml -> Either String MachineDefListenPort
    realizeListenPort NetListenPortYaml {..} =
      maybe
        (Left $ "Protocol not found " ++ nlpyProtocol ++ " in port definition " ++ nlpyName ++ " machine " ++ machname)
        (\np ->
           case List.uncons nlpyLabel of
             (Just (ch,[])) ->
               Right $ MachineDefListenPort
                 { mdpName = nlpyName
                 , mdpChar = ch
                 , mdpProtocol = np
                 , mdpInternal = nlpyPort
                 , mdpExternal = nlpyDefaultHostPort
                 }
             _ ->
               Left $ "Label must be 1 char long in port definition " ++ nlpyName ++ " machine " ++ machname
        )
        (Map.lookup nlpyProtocol protos)

    yamlToRealMachine MachineDefYaml {..} =
      let
        (lpErrors, listenPorts) =
          partitionEithers $ realizeListenPort <$> mdyListenPorts
      in
      if null lpErrors then
        Right $ MachineDef
          { mdName = machname
          , mdTemplate = mdyBaseYaml
          , mdPorts =
              Map.fromList $
                (\mdlp@MachineDefListenPort {..} -> (mdpChar, mdlp)) <$> listenPorts
          , mdConnections = Map.empty
          , mdMounts = Map.empty
          }
      else
        Left $ List.intercalate "," $ show <$> lpErrors

realizeMapOfObjects
  :: forall yaml final.
     Map String yaml
  -> (Map String yaml -> String -> Either String final)
  -> (final -> String)
  -> Either String (Map String final)
realizeMapOfObjects objDefs realizeOneObject getNameOf =
  let
    result = (realizeOneObject objDefs) <$> (Map.keys objDefs)
    (errors, results) = partitionEithers result
  in
  if null errors then
    Right $ Map.fromList $ (\np -> (getNameOf np,np)) <$> results
  else
    Left $ List.intercalate "," $ show <$> errors

realizeProtocolDefs :: Map String NetProtoYaml -> Either String (Map String NetProto)
realizeProtocolDefs protocolDefs =
  realizeMapOfObjects
    protocolDefs
    (realizeProtocolDef Set.empty)
    npName

realizeMachineDefs
  :: Map String NetProto
  -> Map String MachineDefYaml
  -> Either String (Map String MachineDef)
realizeMachineDefs protos machineDefs =
  realizeMapOfObjects
    machineDefs
    (realizeMachineDef protos)
    mdName

parseYaml :: (Aeson.FromJSON a) => String -> Either String a
parseYaml text =
  either
    (Left . show)
    (Right . id)
    (Yaml.decodeEither' (Text.encodeUtf8 $ Text.pack text))
