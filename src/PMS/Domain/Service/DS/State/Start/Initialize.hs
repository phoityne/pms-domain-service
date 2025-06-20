{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module PMS.Domain.Service.DS.State.Start.Initialize where

import Data.Default
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Lens
import qualified Data.Text as T
import qualified Control.Concurrent.STM as STM

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DM.Constant as DM

import PMS.Domain.Service.DM.Type


-- |
--
instance IStateActivity StartStateData InitializeEventData where
  action s (InitializeEvent r@(InitializeEventData dat)) = do
    $logDebugS DM._LOGTAG "Start init called."
    $logDebugS DM._LOGTAG (T.pack (show s))
    $logDebugS DM._LOGTAG (T.pack (show r))

    let jsonRpc = dat^.DM.jsonrpcMcpInitializeRequestData
        resDat  = DM.McpInitializeResponseData jsonRpc def
        res     = DM.McpInitializeResponse resDat

    $logDebugS DM._LOGTAG $ T.pack $ show res

    queue <- view DM.responseQueueDomainData <$> lift ask
    liftIO $ STM.atomically $ STM.writeTQueue queue res

    return Nothing

