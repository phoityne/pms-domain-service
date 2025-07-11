{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PMS.Domain.Service.DM.Type where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Text as T

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DM.Constant as DM

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Key (toText)
import Control.Monad (forM)

--------------------------------------------------------------------------------
-- |
--
newtype JsonObjectMap = JsonObjectMap { unJsonObjectMap :: [(String, String)] }
  deriving (Show)

instance FromJSON JsonObjectMap where
  parseJSON = withObject "JsonObjectMap" $ \o -> do
    listPairs <- forM (KM.toList o) $ \(k, v) -> do
      stringValue <- parseJSON v
      return (T.unpack (toText k), T.unpack stringValue)
    return $ JsonObjectMap listPairs

--------------------------------------------------------------------------------
-- |
--
data StateTransition =
    StartToRun
  | RunToStop
  deriving (Show, Eq)

noStateTransition :: Maybe StateTransition
noStateTransition = Nothing

-- |
--
data EntryEventData       = EntryEventData deriving (Show)
data ExitEventData        = ExitEventData  deriving (Show)
data TransitEventData     = TransitEventData StateTransition deriving (Show)
-- doActibity
data InitializeEventData  = InitializeEventData DM.McpInitializeRequestData deriving (Show)
data InitializedEventData = InitializedEventData DM.McpInitializedNotificationData deriving (Show)
data ToolsListEventData   = ToolsListEventData DM.McpToolsListRequestData deriving (Show)
data ToolsCallEventData   = ToolsCallEventData DM.McpToolsCallRequestData deriving (Show)
data PromptsListEventData = PromptsListEventData DM.McpPromptsListRequestData deriving (Show)
data PromptsGetEventData  = PromptsGetEventData DM.McpPromptsGetRequestData deriving (Show)
data ResourcesTemplatesListEventData = ResourcesTemplatesListEventData DM.McpResourcesTemplatesListRequestData deriving (Show)
data ResourcesListEventData = ResourcesListEventData DM.McpResourcesListRequestData deriving (Show)
data ResourcesReadEventData = ResourcesReadEventData DM.McpResourcesReadRequestData deriving (Show)
data CancelledEventData   = CancelledEventData DM.McpCancelledNotificationData deriving (Show)
data CompletionCompleteEventData = CompletionCompleteEventData DM.McpCompletionCompleteRequestData deriving (Show)


-- |
--
data Event r where
  EntryEvent       :: Event EntryEventData
  ExitEvent        :: Event ExitEventData
  TransitEvent     :: TransitEventData     -> Event TransitEventData
  -- doActibity
  InitializeEvent  :: InitializeEventData  -> Event InitializeEventData
  InitializedEvent :: InitializedEventData -> Event InitializedEventData
  ToolsListEvent   :: ToolsListEventData   -> Event ToolsListEventData
  ToolsCallEvent   :: ToolsCallEventData   -> Event ToolsCallEventData
  PromptsListEvent :: PromptsListEventData -> Event PromptsListEventData
  PromptsGetEvent  :: PromptsGetEventData -> Event PromptsGetEventData
  ResourcesTemplatesListEvent :: ResourcesTemplatesListEventData -> Event ResourcesTemplatesListEventData
  ResourcesListEvent :: ResourcesListEventData -> Event ResourcesListEventData
  ResourcesReadEvent :: ResourcesReadEventData -> Event ResourcesReadEventData
  CancelledEvent   :: CancelledEventData   -> Event CancelledEventData
  CompletionCompleteEvent :: CompletionCompleteEventData -> Event CompletionCompleteEventData

deriving instance Show r => Show (Event r)

-- |
--
data EventW = forall r. EventW (Event r)




--------------------------------------------------------------------------------
-- Type for Domain Service.
--------------------------------------------------------------------------------
-- |
--
type AppStateContext = StateT AppStateW (ReaderT DM.DomainData (ExceptT DM.ErrorData (LoggingT IO)))
type AppContext = AppStateContext

-- type AppStateContext = StateT AppStateW IO

-- |
--
data StartStateData = StartStateData deriving (Show)
data RunStateData  = RunStateData  deriving (Show)
data StopStateData = StopStateData deriving (Show)
data AppState s where
  StartState :: AppState StartStateData
  RunState  :: AppState RunStateData
  StopState :: AppState StopStateData

deriving instance Show s => Show (AppState s)

-- |
--
data AppStateW = forall s. (IAppState s, Show s) => AppStateW (AppState s)


-- |
--
class (Show s, Show r) => IStateActivity s r where
  action :: (AppState s) -> (Event r) -> AppStateContext (Maybe StateTransition)
  action s (TransitEvent (TransitEventData t)) = do
    $logDebugS DM._LOGTAG $ T.pack $ show s ++ " " ++ show t ++ " will transit."
    return (Just t)
  action s r = do
    $logDebugS DM._LOGTAG $ T.pack $ show s ++ " " ++ show r ++ " not supported. will do nothing."
    return Nothing

-- |
--
class IAppState s where
  actionS  :: AppState s -> EventW -> AppStateContext (Maybe StateTransition)

-- |
--
class IAppStateW s where
  actionSW  :: s -> EventW -> AppStateContext (Maybe StateTransition)

instance IAppStateW AppStateW where
  actionSW (AppStateW a) r = actionS a r


