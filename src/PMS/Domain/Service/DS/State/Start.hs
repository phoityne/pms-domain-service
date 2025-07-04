{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module PMS.Domain.Service.DS.State.Start where

import Control.Monad.Logger
import qualified Data.Text as T
import Control.Lens

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DM.Constant as DM

import PMS.Domain.Service.DS.Utility
import PMS.Domain.Service.DM.Type
import PMS.Domain.Service.DM.TH
import PMS.Domain.Service.DS.State.Start.Initialize()
import PMS.Domain.Service.DS.State.Start.Initialized()

{-
-- |
--
instance IAppState StartStateData where
  actionS s (EventW r@EntryEvent{})      = action s r
  actionS s (EventW r@ExitEvent{})       = action s r
  actionS s (EventW r@TransitEvent{})    = action s r
  actionS s (EventW r@InitializeEvent{})       = action s r
  actionS s (EventW r@LaunchEvent{})     = action s r
  actionS s (EventW r@DisconnectEvent{}) = action s r
  actionS s (EventW r@TerminateEvent{})  = action s r
-}
instanceTH_IAppState ''StartStateData

-- |
--
instance IStateActivity StartStateData EntryEventData where
  action _ _ = do
    $logDebugS DM._LOGTAG "Start entry called."
    return noStateTransition

-- |
--
instance IStateActivity StartStateData ExitEventData where
  action _ _ = do
    $logDebugS DM._LOGTAG "Start exit called."
    return noStateTransition

-- |
--
instance IStateActivity StartStateData TransitEventData
  -- @see default implementation in Type module.


-- |
--
{-
instance IStateActivity StartStateData InitializedEventData where
  action _ _ = do
    $logDebugS DM._LOGTAG "initialized called."

    $logDebugS DM._LOGTAG "start watch tools-list.json."
    let jsonRpc = dat^.DM.jsonrpcMcpToolsListRequestData
        cmdDat = DM.ToolsListWatchCommandData jsonRpc
        cmd = DM.ToolsListWatchCommand cmdDat

    wq <- view DM.watchQueueDomainData <$> lift ask
    liftIO $ STM.atomically $ STM.writeTQueue wq cmd
    
    return $ Just StartToRun
-}

-- |
--
instance IStateActivity StartStateData ToolsListEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StartStateData ToolsCallEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StartStateData PromptsListEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StartStateData PromptsGetEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StartStateData ResourcesTemplatesListEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StartStateData ResourcesListEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StartStateData ResourcesReadEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StartStateData CancelledEventData where
  action _ dat = do
    $logInfoS DM._LOGTAG $ T.pack $ "notifications/cancelled called. " ++ show dat
    return noStateTransition

-- |
--
instance IStateActivity StartStateData CompletionCompleteEventData where
  action _ (CompletionCompleteEvent (CompletionCompleteEventData evDat)) = do
    $logInfoS DM._LOGTAG $ T.pack $ "CompletionCompleteEvent called. " ++ show evDat

    sendCompletionResponse $ evDat^.DM.jsonrpcMcpCompletionCompleteRequestData

    return noStateTransition

