{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module PMS.Domain.Service.DS.Core where

import Control.Monad.Logger
import Data.Conduit
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Lens
import Control.Monad.Reader
import qualified Control.Concurrent.STM as STM
import Control.Monad.Trans.State.Lazy
import qualified Data.Text as T
import Control.Monad.Except

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DM.Constant as DM

import PMS.Domain.Service.DM.Type
import PMS.Domain.Service.DM.TH
import PMS.Domain.Service.DS.State.Start()
import PMS.Domain.Service.DS.State.Run()
import PMS.Domain.Service.DS.State.Stop()

-- |
--
funcTH_transit


-- |
--
app :: AppContext ()
app = do
  $logDebugS DM._LOGTAG "app called."
  runConduit pipeline
  where
    pipeline :: ConduitM () Void AppContext ()
    pipeline = src .| work .| sink

---------------------------------------------------------------------------------
-- |
--
src :: ConduitT () DM.McpRequest AppContext ()
src = lift go >>= yield >> src
  where
    go :: AppContext DM.McpRequest
    go = do
      queue <- view DM.requestQueueDomainData <$> lift ask
      req <- liftIO $ STM.atomically $ STM.readTQueue queue
      $logDebugS DM._LOGTAG $ T.pack $ "src: req: " ++ show req
      return req

---------------------------------------------------------------------------------
-- |
--
work :: ConduitT DM.McpRequest EventW AppContext ()
work = await >>= \case
  Just reqBS -> flip catchError errHdl $
    lift (go reqBS) >>= yield >> work
  Nothing -> do
    $logWarnS DM._LOGTAG "work: await returns nothing. skip."
    work
  where
    errHdl :: String -> ConduitT DM.McpRequest EventW AppContext ()
    errHdl msg = do
      $logWarnS DM._LOGTAG $ T.pack $ "work: exception occurred. skip. " ++ msg
      work


    go :: DM.McpRequest -> AppContext EventW
    go req = req2ev req

-- |
--
req2ev :: DM.McpRequest -> AppContext EventW
req2ev (DM.McpInitializeRequest dat) = pure . EventW . InitializeEvent . InitializeEventData $ dat
req2ev (DM.McpToolsListRequest dat) = pure . EventW . ToolsListEvent . ToolsListEventData $ dat
req2ev (DM.McpToolsCallRequest dat) = pure . EventW . ToolsCallEvent . ToolsCallEventData $ dat
req2ev (DM.McpPromptsListRequest dat) = pure . EventW . PromptsListEvent . PromptsListEventData $ dat
req2ev (DM.McpPromptsGetRequest dat) = pure . EventW . PromptsGetEvent . PromptsGetEventData $ dat
req2ev (DM.McpResourcesTemplatesListRequest dat) = pure . EventW . ResourcesTemplatesListEvent . ResourcesTemplatesListEventData $ dat
req2ev (DM.McpResourcesListRequest dat) = pure . EventW . ResourcesListEvent . ResourcesListEventData $ dat
req2ev (DM.McpResourcesReadRequest dat) = pure . EventW . ResourcesReadEvent . ResourcesReadEventData $ dat
req2ev (DM.McpInitializedNotification dat) = pure . EventW . InitializedEvent . InitializedEventData $ dat
req2ev (DM.McpCancelledNotification dat) = pure . EventW . CancelledEvent . CancelledEventData $ dat
req2ev (DM.McpCompletionCompleteRequest dat) = pure . EventW . CompletionCompleteEvent . CompletionCompleteEventData $ dat

---------------------------------------------------------------------------------
-- |
--
sink :: ConduitT EventW Void AppContext ()
sink = await >>= \case
  Just ev -> flip catchError errHdl $ do
    lift (go ev) >> sink
  Nothing -> do
    $logWarnS DM._LOGTAG "sink: await returns nothing. skip."
    sink

  where
    errHdl :: String -> ConduitT EventW Void AppContext ()
    errHdl msg = do
      $logWarnS DM._LOGTAG $ T.pack $ "sink: exception occurred. skip. " ++ msg
      sink
      
    go :: EventW -> AppContext ()
    go ev = get >>= flip actionSW ev >>= \case
      Nothing -> return ()
      Just st -> transit st
