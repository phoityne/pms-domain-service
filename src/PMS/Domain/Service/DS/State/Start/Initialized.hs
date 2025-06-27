{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module PMS.Domain.Service.DS.State.Start.Initialized where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Lens
import qualified Control.Concurrent.STM as STM

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DM.Constant as DM

import PMS.Domain.Service.DM.Type


-- |
--
instance IStateActivity StartStateData InitializedEventData where
  action _ (InitializedEvent (InitializedEventData _)) = do
    $logDebugS DM._LOGTAG "initialized called."

    wq <- view DM.watchQueueDomainData <$> lift ask

    $logDebugS DM._LOGTAG "start watch tools-list.json."
    let toolCmd = DM.ToolsListWatchCommand DM.ToolsListWatchCommandData
    liftIO $ STM.atomically $ STM.writeTQueue wq toolCmd
    
    $logDebugS DM._LOGTAG "start watch prompts-list.json."
    let promptsCmd = DM.PromptsListWatchCommand DM.PromptsListWatchCommandData
    liftIO $ STM.atomically $ STM.writeTQueue wq promptsCmd

    return $ Just StartToRun

