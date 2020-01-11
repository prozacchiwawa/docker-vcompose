module Docker.System
  ( DockerSystem (..)
  , DockerSystemYaml (..)
  , NetProtoYaml (..)
  , MachineDefYaml (..)
  , parseYaml
  , realizeProtocolDefs
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

{-
data MachineDefListenPort = MachineDefListenPort
  { mdpName :: String
  , mdpChar :: Char
  , mdpProtocol :: NetProto
  , mdpInternal :: Int
  , mdpExternal :: Int
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
  , mdMounts :: Map Char MachineDefMount
  , mdConnections :: Map Char MachineDefConn
  }
  deriving (Show, Eq, Ord)
-}

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
  { nlpName :: String
  , nlpLabel :: String
  , nlpProtocol :: String
  }
  deriving (Show, Generic)

instance Aeson.FromJSON NetListenPortYaml

data NetConnectPortYaml = NetConnectPortYaml
  { ncpName :: String
  , ncpLabel :: String
  , ncpProtocol :: String
  }
  deriving (Show, Generic)

instance Aeson.FromJSON NetConnectPortYaml

data MachineDefYaml = MachineDefYaml
  { mdyBaseYaml :: String
  , mdyListenPorts :: [NetListenPortYaml]
  , mdyConnectPorts :: [NetConnectPortYaml]
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

realizeProtocolDefs :: Map String NetProtoYaml -> Either String (Map String NetProto)
realizeProtocolDefs protocolDefs =
  let
    result = (realizeProtocolDef Set.empty protocolDefs) <$> (Map.keys protocolDefs)
    (errors, results) = partitionEithers result
  in
  if null errors then
    Right $ Map.fromList $ (\np@NetProto {..} -> (npName,np)) <$> results
  else
    Left $ List.intercalate "," $ show <$> errors

parseYaml :: (Aeson.FromJSON a) => String -> Either String a
parseYaml text =
  either
    (Left . show)
    (Right . id)
    (Yaml.decodeEither' (Text.encodeUtf8 $ Text.pack text))
