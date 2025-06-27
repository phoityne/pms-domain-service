{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module PMS.Domain.Service.DS.State.Run.PromptsList where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Logger
import qualified Data.Text as T
import Control.Lens
import Control.Monad.Reader
import qualified Control.Concurrent.STM as STM
import System.FilePath
import System.Directory
import Control.Monad.Except
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DM.Constant as DM

import PMS.Domain.Service.DM.Type
import qualified PMS.Domain.Service.DS.Utility as U


-- |
--
instance IStateActivity RunStateData PromptsListEventData where
  action _ (PromptsListEvent (PromptsListEventData dat)) = flip catchError errHdl $ do
    $logDebugS DM._LOGTAG "Run PromptsListEvent called."

    promptsDir <- view DM.promptsDirDomainData <$> lift ask

    let promptsFile = promptsDir </> DM._PROMPTS_LIST_FILE
    exists <- liftIO $ doesFileExist promptsFile
    cont <- if exists
              then U.readFile promptsFile
              else do
                $logInfoS DM._LOGTAG $ T.pack $ "file not found." ++ promptsFile
                return "[]"

    response cont
    return noStateTransition

    where
      errHdl :: String -> AppContext (Maybe StateTransition)
      errHdl msg = do
        $logErrorS DM._LOGTAG $ T.pack $ "PromptsListEventData: exception occurred. " ++ msg
        response $ BL8.pack msg
        return noStateTransition

      response :: BL.ByteString -> AppContext ()
      response cont = do
        let result = DM.McpPromptsListResponseResult $ DM.RawJsonByteString cont
            jsonRpc = dat^.DM.jsonrpcMcpPromptsListRequestData
            resDat = DM.McpPromptsListResponseData jsonRpc result
            res = DM.McpPromptsListResponse resDat

        $logDebugS DM._LOGTAG $ T.pack $ show res

        queue <- view DM.responseQueueDomainData <$> lift ask
        liftIO $ STM.atomically $ STM.writeTQueue queue res
