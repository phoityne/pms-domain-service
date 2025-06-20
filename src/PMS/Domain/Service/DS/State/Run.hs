{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module PMS.Domain.Service.DS.State.Run where

import Control.Monad.Logger

import qualified PMS.Domain.Model.DM.Constant as DM

import PMS.Domain.Service.DM.Type
import PMS.Domain.Service.DM.TH

import PMS.Domain.Service.DS.State.Run.ToolsList()
import PMS.Domain.Service.DS.State.Run.ToolsCall()


instanceTH_IAppState ''RunStateData

-- |
--
instance IStateActivity RunStateData EntryEventData where
  action _ _ = do
    $logDebugS DM._LOGTAG "Run entry called."
    return Nothing

-- |
--
instance IStateActivity RunStateData ExitEventData where
  action _ _ = do
    $logDebugS DM._LOGTAG "Run exit called."
    return Nothing

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
instance IStateActivity RunStateData LaunchEventData where
  action _ _ = do
    $logDebugS DM._LOGTAG "Run launch called."
    return Nothing

-- |
--
instance IStateActivity RunStateData DisconnectEventData where
  action _ _ = do
    $logDebugS DM._LOGTAG "Run discoonect called."
    return $ Just RunToStop

-- |
--
instance IStateActivity RunStateData TerminateEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity RunStateData InitializedEventData
  -- @see default implementation in Type module.
