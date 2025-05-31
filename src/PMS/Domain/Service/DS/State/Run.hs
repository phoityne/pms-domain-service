{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module PMS.Domain.Service.DS.State.Run where


import System.IO
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Logger
import qualified Data.Text as T
import Control.Lens
import Control.Monad.Reader
import qualified Control.Concurrent.STM as STM
import System.FilePath
import System.Exit

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DM.Constant as DM

import PMS.Domain.Service.DM.Type
import PMS.Domain.Service.DM.TH
import PMS.Domain.Service.DM.Constant
import qualified PMS.Domain.Service.DS.Utility as U



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
instance IStateActivity RunStateData InitEventData
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

-- |
--
instance IStateActivity RunStateData ToolsListEventData where
  action s (ToolsListEvent r@(ToolsListEventData dat)) = do
    $logDebugS DM._LOGTAG "Run ToolsListEvent called."
    $logDebugS DM._LOGTAG (T.pack (show s))
    $logDebugS DM._LOGTAG (T.pack (show r))

    scriptsDir <- view DM.scriptsDirDomainData <$> lift ask

    let toolFile = scriptsDir </> _TOOLS_LIST_FILE
    cont <- U.readFile toolFile
    let result = DM.McpToolsListResponseResult $ DM.RawJsonByteString cont
        jsonRpc = dat^.DM.jsonrpcMcpToolsListRequestData
        resDat = DM.McpToolsListResponseData jsonRpc result
        res = DM.McpToolsListResponse resDat

    $logDebugS DM._LOGTAG $ T.pack $ show res

    queue <- view DM.responseQueueDomainData <$> lift ask
    liftIO $ STM.atomically $ STM.writeTQueue queue res

    return noStateTransition

-- |
--
instance IStateActivity RunStateData ToolsCallEventData where
  action _ (ToolsCallEvent (ToolsCallEventData evDat)) = do
    $logDebugS DM._LOGTAG "Run ToolsCallEvent called."

    cmd <- genCommand evDat $ evDat^.DM.paramsMcpToolsCallRequestData^.DM.nameMcpToolsCallRequestDataParams

    cmdQ <- view DM.commandQueueDomainData <$> lift ask
    liftIO $ STM.atomically $ STM.writeTQueue cmdQ cmd

    return noStateTransition
    
    where
      genCommand :: DM.McpToolsCallRequestData -> String -> AppContext DM.Command
      genCommand dat "pty-connect" = genPtyConnectCommand dat
      genCommand dat "pty-bash"    = genPtyConnectCommand dat
      genCommand dat "pty-ssh"     = genPtyConnectCommand dat
      genCommand dat "pty-cabal"   = genPtyConnectCommand dat
      genCommand dat "pty-stack"   = genPtyConnectCommand dat
      genCommand dat "pty-ghci"    = genPtyConnectCommand dat
      genCommand dat "pty-message" = do
        resQ <- view DM.responseQueueDomainData <$> lift ask
        let cmdDat = DM.PtyMessageCommandData {
                      DM._namePtyMessageCommandData = dat^.DM.paramsMcpToolsCallRequestData^.DM.nameMcpToolsCallRequestDataParams
                    , DM._argumentsPtyMessageCommandData = dat^.DM.paramsMcpToolsCallRequestData^.DM.argumentsMcpToolsCallRequestDataParams
                    , DM._callbackPtyMessageCommandData = callback resQ
                    }

        return $ DM.PtyMessageCommand cmdDat
      genCommand dat _ = do
        resQ <- view DM.responseQueueDomainData <$> lift ask
        let cmdDat = DM.SystemCommandData {
                      DM._nameSystemCommandData = dat^.DM.paramsMcpToolsCallRequestData^.DM.nameMcpToolsCallRequestDataParams
                    , DM._argumentsSystemCommandData = dat^.DM.paramsMcpToolsCallRequestData^.DM.argumentsMcpToolsCallRequestDataParams
                    , DM._callbackSystemCommandData = callback resQ
                    }

        return $ DM.SystemCommand cmdDat

      genPtyConnectCommand :: DM.McpToolsCallRequestData -> AppContext DM.Command
      genPtyConnectCommand dat = do
        resQ <- view DM.responseQueueDomainData <$> lift ask
        let cmdDat = DM.PtyConnectCommandData {
                      DM._namePtyConnectCommandData = dat^.DM.paramsMcpToolsCallRequestData^.DM.nameMcpToolsCallRequestDataParams
                    , DM._argumentsPtyConnectCommandData = dat^.DM.paramsMcpToolsCallRequestData^.DM.argumentsMcpToolsCallRequestDataParams
                    , DM._callbackPtyConnectCommandData = callback resQ
                    }

        return $ DM.PtyConnectCommand cmdDat

      -- |
      --
      callback :: STM.TQueue DM.McpResponse -> DM.SystemCommandCallback ()
      callback resQ code outStr errStr = do
        hPutStrLn stderr $ "[INFO] PMS.Domain.Service.DS.State.Run.callback called."

        let jsonRpc = evDat^.DM.jsonrpcMcpToolsCallRequestData
            content = [ DM.McpToolsCallResponseResultContent "text" outStr
                      , DM.McpToolsCallResponseResultContent "text" errStr
                      ]
            result = DM.McpToolsCallResponseResult {
                       DM._contentMcpToolsCallResponseResult = content
                     , DM._isErrorMcpToolsCallResponseResult = (ExitSuccess /= code)
                     }
            resDat = DM.McpToolsCallResponseData jsonRpc result
            res = DM.McpToolsCallResponse resDat

        STM.atomically $ STM.writeTQueue resQ res

        hPutStrLn stderr $ "[INFO] PMS.Domain.Service.DS.State.Run.callback end."
