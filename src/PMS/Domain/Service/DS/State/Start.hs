{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module PMS.Domain.Service.DS.State.Start where

import Control.Monad.Logger

import qualified PMS.Domain.Model.DM.Constant as DM

import PMS.Domain.Service.DM.Type
import PMS.Domain.Service.DM.TH
import PMS.Domain.Service.DS.State.Start.Initialize()
import PMS.Domain.Service.DS.State.Start.Initialized()
import PMS.Domain.Service.DS.State.Start.Launch()
import PMS.Domain.Service.DS.State.Start.Disconnect()
import PMS.Domain.Service.DS.State.Start.Terminate()

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
    return Nothing

-- |
--
instance IStateActivity StartStateData ExitEventData where
  action _ _ = do
    $logDebugS DM._LOGTAG "Start exit called."
    return Nothing

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
