{-# LANGUAGE TemplateHaskell #-}

module PMS.Domain.Service.DM.Type where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Text as T

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DM.Constant as DM


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
data InitEventData        = InitEventData DM.McpInitializeRequestData deriving (Show)
data InitializedEventData = InitializedEventData DM.McpInitializedNotificationData deriving (Show)
data ToolsListEventData   = ToolsListEventData DM.McpToolsListRequestData deriving (Show)
data ToolsCallEventData   = ToolsCallEventData DM.McpToolsCallRequestData deriving (Show)
data LaunchEventData      = LaunchEventData Int deriving (Show)
data DisconnectEventData  = DisconnectEventData Int deriving (Show)
data TerminateEventData   = TerminateEventData Int  deriving (Show)

-- |
--
data Event r where
  EntryEvent       :: Event EntryEventData
  ExitEvent        :: Event ExitEventData
  TransitEvent     :: TransitEventData     -> Event TransitEventData
  -- doActibity
  InitEvent        :: InitEventData        -> Event InitEventData
  InitializedEvent :: InitializedEventData -> Event InitializedEventData
  ToolsListEvent   :: ToolsListEventData   -> Event ToolsListEventData
  ToolsCallEvent   :: ToolsCallEventData   -> Event ToolsCallEventData
  LaunchEvent      :: LaunchEventData      -> Event LaunchEventData
  DisconnectEvent  :: DisconnectEventData  -> Event DisconnectEventData
  TerminateEvent   :: TerminateEventData   -> Event TerminateEventData

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


