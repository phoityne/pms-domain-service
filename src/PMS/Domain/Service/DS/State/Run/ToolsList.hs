{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module PMS.Domain.Service.DS.State.Run.ToolsList where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Logger
import qualified Data.Text as T
import Control.Lens
import Control.Monad.Reader
import qualified Control.Concurrent.STM as STM
import System.FilePath

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DM.Constant as DM

import PMS.Domain.Service.DM.Type
import qualified PMS.Domain.Service.DS.Utility as U


-- |
--
instance IStateActivity RunStateData ToolsListEventData where
  action s (ToolsListEvent r@(ToolsListEventData dat)) = do
    $logDebugS DM._LOGTAG "Run ToolsListEvent called."
    $logDebugS DM._LOGTAG (T.pack (show s))
    $logDebugS DM._LOGTAG (T.pack (show r))

    toolsDir <- view DM.toolsDirDomainData <$> lift ask

    let toolFile = toolsDir </> DM._TOOLS_LIST_FILE
    cont <- U.readFile toolFile
    let result = DM.McpToolsListResponseResult $ DM.RawJsonByteString cont
        jsonRpc = dat^.DM.jsonrpcMcpToolsListRequestData
        resDat = DM.McpToolsListResponseData jsonRpc result
        res = DM.McpToolsListResponse resDat

    $logDebugS DM._LOGTAG $ T.pack $ show res

    queue <- view DM.responseQueueDomainData <$> lift ask
    liftIO $ STM.atomically $ STM.writeTQueue queue res

    return noStateTransition
