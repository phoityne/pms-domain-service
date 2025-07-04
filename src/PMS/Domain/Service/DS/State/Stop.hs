{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module PMS.Domain.Service.DS.State.Stop where

import Control.Monad.Logger
import qualified Data.Text as T
import Control.Lens

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DM.Constant as DM

import PMS.Domain.Service.DS.Utility
import PMS.Domain.Service.DM.Type
import PMS.Domain.Service.DM.TH

-- |
--
instanceTH_IAppState ''StopStateData

-- |
--
instance IStateActivity StopStateData EntryEventData where
  action _ _ = do
    $logDebugS DM._LOGTAG "Stop entry called."
    return noStateTransition

-- |
--
instance IStateActivity StopStateData ExitEventData where
  action _ _ = do
    $logDebugS DM._LOGTAG "Stop exit called."
    return noStateTransition

-- |
--
instance IStateActivity StopStateData TransitEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StopStateData InitializeEventData
  -- @see default implementation in Type module.

-- |
--
-- instance IStateActivity StopStateData LaunchEventData
  -- @see default implementation in Type module.

-- |
--
-- instance IStateActivity StopStateData DisconnectEventData
  -- @see default implementation in Type module.

-- |
--
{-
instance IStateActivity StopStateData TerminateEventData where
  action _ _ = do
    $logDebugS DM._LOGTAG "Stop terminate called. will exit."
    return Nothing
-}
-- |
--
instance IStateActivity StopStateData InitializedEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StopStateData ToolsListEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StopStateData ToolsCallEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StopStateData PromptsListEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StopStateData PromptsGetEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StopStateData ResourcesTemplatesListEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StopStateData ResourcesListEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StopStateData ResourcesReadEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StopStateData CancelledEventData where
  action _ dat = do
    $logInfoS DM._LOGTAG $ T.pack $ "notifications/cancelled called. " ++ show dat
    return noStateTransition


-- |
--
instance IStateActivity StopStateData CompletionCompleteEventData where
  action _ (CompletionCompleteEvent (CompletionCompleteEventData evDat)) = do
    $logInfoS DM._LOGTAG $ T.pack $ "CompletionCompleteEvent called. " ++ show evDat

    sendCompletionResponse $ evDat^.DM.jsonrpcMcpCompletionCompleteRequestData

    return noStateTransition


