module Docker.System
  ( DockerSystem (..)
  , DockerSystemYaml (..)
  , NetProtoYaml (..)
  , MachineDefYaml (..)
  , parseYaml
  )
where

import GHC.Generics

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Yaml as Yaml

import Util.Glyph

{-
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

data DockerProtoDefsYaml = DockerProtoDefsYaml
  { dpdProtocols :: [NetProtoYaml]
  }
  deriving (Show)

parseYaml :: (Aeson.FromJSON a) => String -> Either String a
parseYaml text =
  either
    (Left . show)
    (Right . id)
    (Yaml.decodeEither' (Text.encodeUtf8 $ Text.pack text))
