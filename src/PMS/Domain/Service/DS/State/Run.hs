{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module PMS.Domain.Service.DS.State.Run where

import Control.Monad.Logger
import qualified Data.Text as T
import Control.Lens

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DM.Constant as DM

import PMS.Domain.Service.DM.Type
import PMS.Domain.Service.DM.TH
import PMS.Domain.Service.DS.Utility
import PMS.Domain.Service.DS.State.Run.ToolsList()
import PMS.Domain.Service.DS.State.Run.ToolsCall()
import PMS.Domain.Service.DS.State.Run.PromptsList()
import PMS.Domain.Service.DS.State.Run.PromptsGet()
import PMS.Domain.Service.DS.State.Run.ResourcesTemplatesList()
import PMS.Domain.Service.DS.State.Run.ResourcesList()
import PMS.Domain.Service.DS.State.Run.ResourcesRead()


instanceTH_IAppState ''RunStateData

-- |
--
instance IStateActivity RunStateData EntryEventData where
  action _ _ = do
    $logDebugS DM._LOGTAG "Run entry called."
    return noStateTransition

-- |
--
instance IStateActivity RunStateData ExitEventData where
  action _ _ = do
    $logDebugS DM._LOGTAG "Run exit called."
    return noStateTransition

-- |
--
instance IStateActivity RunStateData TransitEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity RunStateData InitializeEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity RunStateData InitializedEventData
  -- @see default implementation in Type module.


-- |
--
instance IStateActivity RunStateData CancelledEventData where
  action _ dat = do
    $logInfoS DM._LOGTAG $ T.pack $ "notifications/cancelled called. " ++ show dat
    return noStateTransition


-- |
--
instance IStateActivity RunStateData CompletionCompleteEventData where
  action _ (CompletionCompleteEvent (CompletionCompleteEventData evDat)) = do
    $logInfoS DM._LOGTAG $ T.pack $ "CompletionCompleteEvent called. " ++ show evDat

    sendCompletionResponse $ evDat^.DM.jsonrpcMcpCompletionCompleteRequestData

    return noStateTransition

