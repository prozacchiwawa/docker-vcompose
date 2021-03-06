module Docker.System
  ( DockerSystem (..)
  , DockerSystemYaml (..)
  , NetProtoYaml (..)
  , MachineDefYaml (..)
  , MachineDef (..)
  , parseYaml
  , realizeProtocolDefs
  , realizeMachineDefs
  , assembleSystem
  , createSystemYaml
  )
where

import GHC.Generics

import qualified Control.Error.Util as CE
import Control.Monad
import Control.Monad.Trans.Except

import qualified Data.Aeson as Aeson
import Data.Either
import Data.Glyphic.Glyph
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.List.Split as List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.Scientific
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import qualified Data.Yaml as Yaml

import Util.Aeson

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
  }
  deriving (Show, Eq, Ord)

data MachineDefMount = MachineDefMount
  { mdmName :: String
  , mdmChar :: Char
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
  , mdTemplate :: Aeson.Value
  , mdPorts :: Map Char MachineDefListenPort
  , mdConnections :: Map Char MachineDefConn
  }
  deriving (Show, Eq)

data MachineConnTarget m = MachineConnTarget
  { mctTargetMachine :: m
  , mctTargetListener :: MachineDefListenPort
  }
  deriving (Show, Eq, Ord)

data MachineConn a = MachineConn
  { mcDefinition :: MachineDefConn
  , mcTarget :: Maybe (MachineConnTarget a)
  }
  deriving (Show, Eq, Ord)

data Machine = Machine
  { mName :: String
  , mTemplate :: Aeson.Value
  , mGlyphData :: Aeson.Value
  , mNetwork :: String
  , mListenPorts :: Map Char MachineDefListenPort
  , mConnectPorts :: Map Char (MachineConn Machine)
  }
  deriving (Show, Eq)

data DockerSystem = DockerSystem
  { dsMachineDefs :: Map String MachineDef
  , dsProtocols :: Map String NetProto
  , dsDrawing :: GlyphDrawing Aeson.Value
  , dsEnvironment :: Map String String
  , dsMachineInstances :: Map String Machine
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

data MachineDefYaml = MachineDefYaml
  { mdyBaseYaml :: String
  , mdyListenPorts :: [NetListenPortYaml]
  , mdyConnectPorts :: [NetConnectPortYaml]
  }
  deriving (Show, Generic)

instance Aeson.FromJSON MachineDefYaml

data DockerSystemYaml = DockerSystemYaml
  { dsySourceDirs :: [String]
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
                 }
             _ ->
               Left $ "Label must be 1 char long in port definition " ++ nlpyName ++ " machine " ++ machname
        )
        (Map.lookup nlpyProtocol protos)

    realizeConnectPort :: NetConnectPortYaml -> Either String MachineDefConn
    realizeConnectPort NetConnectPortYaml {..} =
      maybe
        (Left $ "No proto matching " ++ ncpyProtocol ++ " for connection " ++ ncpyName)
        (\np ->
           case List.uncons ncpyLabel of
             (Just (ch,[])) ->
               Right $ MachineDefConn
                 { mdcName = ncpyName
                 , mdcChar = ch
                 , mdcProtocol = np
                 }
             _ -> Left $ "Label must be 1 char long in connect def " ++ ncpyName ++ " machine " ++ machname
        )
        (Map.lookup ncpyProtocol protos)

    yamlToRealMachine MachineDefYaml {..} = do
      let
        (lpErrors, listenPorts) =
          partitionEithers $ realizeListenPort <$> mdyListenPorts

        (cpErrors, connectPorts) =
          partitionEithers $ realizeConnectPort <$> mdyConnectPorts

      parsedYaml <- parseYaml mdyBaseYaml

      if null lpErrors && null cpErrors then
        Right $ MachineDef
          { mdName = machname
          , mdTemplate = parsedYaml
          , mdPorts =
              Map.fromList $
                (\mdlp@MachineDefListenPort {..} -> (mdpChar, mdlp)) <$> listenPorts
          , mdConnections =
              Map.fromList $
                (\mdc@MachineDefConn {..} -> (mdcChar, mdc)) <$> connectPorts
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

getNetworkYaml :: GlyphDrawing Aeson.Value -> Either String Aeson.Value
getNetworkYaml gd@GlyphDrawing {..} = do
  (networks,tl) <-
    CE.note "No glyph exists with networks key" $
    List.uncons $
    catMaybes $
    (\(gid,gc@GlyphContent {..}) -> getTopLevelValue "networks" gData) <$> Map.toList gGlyphs

  if null tl then
    pure networks
  else
    Left "Multiple glyphs have a toplevel networks key"

getDefaultNetwork :: Aeson.Value -> Either String String
getDefaultNetwork (Aeson.Object o) =
  CE.note "networks array was empty or not array of strings" $
  (Text.unpack . fst) <$> List.uncons (HashMap.keys o)
getDefaultNetwork _ = Left "networks yaml isn't an object"

data InProgressConnectionType
  = IsListenPort Machine GlyphVertex MachineDefListenPort
  | IsOutbound Machine GlyphVertex (MachineConn Machine)
  deriving (Show, Eq)

matchesListenPort :: InProgressConnectionType -> Bool
matchesListenPort (IsListenPort _ _ _) = True
matchesListenPort _ = False

assembleSystem
  :: GlyphDrawing Aeson.Value
  -> DockerSystemYaml
  -> Map String MachineDef
  -> Map String NetProto
  -> Map String String
  -> Either String DockerSystem
assembleSystem gd@GlyphDrawing {..} DockerSystemYaml {..} machines protos env = do
  networkYaml <- getNetworkYaml gd

  let
    (miErrors, machineInstances) =
      partitionEithers $ createBareMachineInstance <$> glyphMachines

  if null miErrors then do
    let
      basicSystem =
        DockerSystem
          { dsMachineDefs = machines
          , dsProtocols = protos
          , dsDrawing = gd
          , dsMachineInstances =
              Map.fromList $ ((\m -> (mName m,m)) . snd) <$> machineInstances
          , dsEnvironment = env
          }

      machineByGlyph = Map.fromList machineInstances

    connectedSystem <-
      foldM (performConnections gd machineByGlyph) basicSystem $ Map.toList gGlyphs

    pure connectedSystem
  else
    Left $ List.intercalate "," $ show <$> miErrors

  where
    glyphList = Map.toList gGlyphs
    glyphMachines = catMaybes $ (uncurry associateMachineDef) <$> glyphList

    createBareMachineInstance
      :: (GlyphId, GlyphContent Aeson.Value, String)
      -> Either String (GlyphId, Machine)
    createBareMachineInstance (gid, gc@GlyphContent {..}, machname) = do
      lookedUpMachine <-
        CE.note ("Cannot find machine " ++ machname ++ " in glyph " ++ show gid) $
        Map.lookup machname machines
      machineName <-
        CE.note ("No id given for machine " ++ show gid) $
        getTopLevelBinding "id" gData
      networkYaml <- getNetworkYaml gd
      useNetwork <- getDefaultNetwork networkYaml

      pure $
        ( gid
        , Machine
            { mName = machineName
            , mTemplate = mdTemplate lookedUpMachine
            , mGlyphData = gData
            , mNetwork = useNetwork
            , mListenPorts = mdPorts lookedUpMachine
            , mConnectPorts = (\m -> MachineConn m Nothing) <$> mdConnections lookedUpMachine
            }
        )

    associateMachineDef
      :: GlyphId
      -> GlyphContent Aeson.Value
      -> Maybe (GlyphId, GlyphContent Aeson.Value, String)
    associateMachineDef gid gc@GlyphContent {..} = do
      machname <- getTopLevelBinding "machine" gData
      pure (gid, gc, machname)

    getConnectionType
      :: Map GlyphId Machine
      -> GlyphVertex
      -> Either String InProgressConnectionType
    getConnectionType machines gvtx@(GlyphVertex {..}) = do
      machine <-
        CE.note ("machine lookup failed for glyph id " ++ show gToGlyph) $
        Map.lookup gToGlyph machines

      let
        connectionAmongListen = Map.lookup gConn $ mListenPorts machine
        connectionAmongOutbound = Map.lookup gConn $ mConnectPorts machine

      case (connectionAmongListen, connectionAmongOutbound) of
        (Just listen, Just outbound) ->
          Left $ "Problem: same letter is used for listen and outbound " ++ show gvtx
        (Just listen, _) -> Right $ IsListenPort machine gvtx listen
        (_, Just outbound) -> Right $ IsOutbound machine gvtx outbound
        _ -> Left $ "Problem: no letter matching " ++ show gvtx

    getConnectionTypeList
      :: Map GlyphId Machine
      -> [GlyphVertex]
      -> Either String [InProgressConnectionType]
    getConnectionTypeList machines =
      foldM
        (\lst vtx -> do
          resultType <- getConnectionType machines vtx
          pure $ resultType : lst
        ) []

    protocolSupersetsOther :: NetProto -> NetProto -> Bool
    protocolSupersetsOther npa npb = npa == npb || Set.member npb (npSpecializes npa)

    addConnectionToMachine :: Char -> Machine -> MachineDefListenPort -> Machine -> Machine
    addConnectionToMachine portlabel targetMachine targetListenPort m@(Machine {..}) =
      let
        newConnectPorts =
          Map.update
            (\mc ->
               Just $ mc { mcTarget = Just $ MachineConnTarget targetMachine targetListenPort }
            )
            portlabel
            mConnectPorts
      in
      m { mConnectPorts = newConnectPorts }

    setOutboundConnection
      :: String -> Char -> Machine -> MachineDefListenPort -> DockerSystem -> DockerSystem
    setOutboundConnection machname portlabel targetMachine targetListenPort system@(DockerSystem {..}) =
      let
        machines =
          Map.update
            (Just . addConnectionToMachine portlabel targetMachine targetListenPort)
            machname
            dsMachineInstances
      in
      system { dsMachineInstances = machines }

    replaceConnection
      :: InProgressConnectionType
      -> DockerSystem
      -> InProgressConnectionType
      -> Either String DockerSystem
    replaceConnection l@(IsOutbound _ _ _) s o =
      Left $ "Outbound connection " ++ show l ++ " is targeted by " ++ show o
    replaceConnection l s o@(IsListenPort _ _ _) =
      Left $ "Inbound connection " ++ show o ++ " is asked to connect to " ++ show l
    replaceConnection l@(IsListenPort lmachine lvtx lport) system o@(IsOutbound omachine ovtx odef) =
      if protocolSupersetsOther (mdpProtocol lport) (mdcProtocol $ mcDefinition odef) then
        Right $ setOutboundConnection
          (mName omachine) (mdcChar $ mcDefinition odef) lmachine lport system
      else
        Left $ "Incompatible " ++ show o ++ " asked to connect to " ++ show l

    replaceConnections
      :: InProgressConnectionType
      -> DockerSystem
      -> [InProgressConnectionType]
      -> Either String DockerSystem
    replaceConnections l = foldM (replaceConnection l)

    performConnection
      :: GlyphDrawing Aeson.Value
      -> Map GlyphId Machine
      -> GlyphContent Aeson.Value
      -> DockerSystem
      -> (GlyphVertex, Set GlyphVertex)
      -> Either String DockerSystem
    performConnection drawing machineByGlyph gc@GlyphContent {..} system (gvtx@GlyphVertex {..}, connectedTo) = do
      firstMachine <-
        CE.note ("Lookup failed for glyph id (to) " ++ show gToGlyph) $
        Map.lookup gToGlyph machineByGlyph

      typeAtOrigin <- getConnectionType machineByGlyph gvtx
      connTypeList <- getConnectionTypeList machineByGlyph $ Set.toList connectedTo

      let
        allConnected = typeAtOrigin : connTypeList

        -- Check for more than one listener
        connectedListeners = filter matchesListenPort allConnected
        connectedOutbounds = filter (not . matchesListenPort) allConnected

        unconsListener = List.uncons connectedListeners

      listener <-
         case unconsListener of
           Just (l, []) -> Right l
           Just (_, _) ->
             Left $ "More than one listener is in net " ++ show gvtx ++ " with " ++ show connectedTo
           _ ->
             Left $ "Each net must have one listener " ++ show gvtx ++ " with " ++ show connectedTo

      replaceConnections listener system connectedOutbounds

    performConnections
      :: GlyphDrawing Aeson.Value
      -> Map GlyphId Machine
      -> DockerSystem
      -> (GlyphId, GlyphContent Aeson.Value)
      -> Either String DockerSystem
    performConnections drawing@GlyphDrawing {..} machineByGlyph system (gid, gc@GlyphContent {..}) =
      foldM (performConnection drawing machineByGlyph gc) system $
      filter (\(vtx,conns) -> gToGlyph vtx == gid) $ Map.toList gNets

getEnvVar :: GlyphDrawing Aeson.Value -> DockerSystem -> Machine -> String -> Either String String
getEnvVar drawing system m ident =
  CE.note ("No such replacement variable " ++ ident ++ " provided, required by " ++ mName m) $
  Map.lookup ident $ dsEnvironment system

queryVariableFromMachine
  :: GlyphDrawing Aeson.Value -> DockerSystem -> Machine -> String -> Either String String
queryVariableFromMachine drawing system m ident = do
  case getTopLevelBinding ident (mGlyphData m) of
    Just v -> Right v
    _ ->
      let
        splitOnDots = List.splitOn "." ident
      in
      case splitOnDots of
        [listener,"port","internal"] ->
          CE.note
            ("Could not find listening port for " ++ listener ++ " in machine " ++ mName m) $
          (show . mdpInternal . fst) <$>
          (List.uncons $
            filter (\MachineDefListenPort {..} -> mdpName == listener) $
            Map.elems $ mListenPorts m)

        [connect,"port"] ->
          let
            defs =
              filter (\MachineConn {..} -> mdcName mcDefinition == connect) $
              Map.elems $ mConnectPorts m
          in
          CE.note
            ("Could not find connection for " ++ connect ++ " in machine " ++ mName m) $
          (show . mdpInternal) <$>
          (mctTargetListener <$>
           (mcTarget =<<
            fst <$>
            (List.uncons defs)
           )
          )

        [connect,"host"] ->
          CE.note
            ("Could not find connection for " ++ connect ++ " in machine " ++ mName m) $
          ((mName . mctTargetMachine) <$>
            (mcTarget =<<
             fst <$>
             (List.uncons $
              filter (\MachineConn {..} -> mdcName mcDefinition == connect) $
              Map.elems $ mConnectPorts m)
            )
          )

        _ -> getEnvVar drawing system m ident

makeDependsSet :: Machine -> Aeson.Value
makeDependsSet m =
  let
    depSet = Set.fromList $ (\MachineConnTarget {..} -> mName mctTargetMachine) <$> catMaybes (mcTarget <$> Map.elems (mConnectPorts m))
  in
  Aeson.Array $ Vector.fromList $ (Aeson.String . Text.pack) <$> Set.toList depSet

-- | If an external port is defined in the glyph yaml that matches a listen port we have,
-- give a string that maps it to the port indicated.
emitPortIfDefined :: Machine -> String -> Aeson.Value -> Maybe String
emitPortIfDefined Machine {..} name v =
  let
    stringedPort =
      case v of
        Aeson.Number n ->
          let
            i :: Maybe Int = toBoundedInteger n
          in
          show <$> i
        Aeson.String s -> Just $ Text.unpack s
  in
  (\sp ->
      ((\MachineDefListenPort {..} -> sp ++ ":" ++ show mdpInternal) . fst) <$>
      List.uncons
      (filter (\MachineDefListenPort {..} -> mdpName == name) $ Map.elems mListenPorts)
  ) =<< stringedPort

makeExternalPorts :: Machine -> Aeson.Value -> Aeson.Value
makeExternalPorts m v =
  let
    keys = either (const []) id $ keysOfDict v
  in
  Aeson.Array $ Vector.fromList $
    (Aeson.String . Text.pack) <$>
    catMaybes
      (
        (\key ->
           case List.splitOn "." key of
             [name,"port","external"] ->
               emitPortIfDefined m name =<< getTopLevelValue key v
             _ -> Nothing
        ) <$> keys
      )

machineToServiceEntry
  :: GlyphDrawing Aeson.Value -> DockerSystem -> Machine -> Either String Aeson.Value
machineToServiceEntry gd system m =
  let
    netYaml = getNetworkYaml gd
    networks =
      either
        (const $ Aeson.Array $ Vector.fromList [Aeson.String $ Text.pack "basic"])
        (Aeson.Array . Vector.fromList . fmap (Aeson.String . Text.pack))
        (keysOfDict =<< netYaml)
  in
  identifyCheckReplaceVariables (queryVariableFromMachine gd system m) $
    addKey "networks" networks $
    addKey "depends_on" (makeDependsSet m) $
    addKey "ports" (makeExternalPorts m $ mGlyphData m) $
    mTemplate m

createSystemYaml :: GlyphDrawing Aeson.Value -> DockerSystem -> Either String Aeson.Value
createSystemYaml gd system@(DockerSystem {..}) = do
  let
    machineInstances = Map.elems dsMachineInstances

  servicesRaw <-
    traverse
       (\m -> do
          serviceEntry <- machineToServiceEntry gd system m
          pure (Text.pack (mName m), serviceEntry)
       ) machineInstances

  let
    services = Aeson.Object $ HashMap.fromList servicesRaw

    networks =
      either
        (const $ Aeson.Object $ HashMap.fromList $ [(Text.pack "basic", emptyObject)])
        id
        (getNetworkYaml gd)

    baseYaml = emptyObject

  pure $
    addKey "services" services $
    addKey "networks" networks $
    addKey "version" (Aeson.String $ Text.pack "2") $ baseYaml
