{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module PMS.Domain.Service.DS.State.Run.ToolsCall where


import System.IO
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Logger
import Control.Lens
import Control.Monad.Reader
import qualified Control.Concurrent.STM as STM
import System.Exit

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DM.Constant as DM

import PMS.Domain.Service.DM.Type


-- |
--
instance IStateActivity RunStateData ToolsCallEventData where
  action _ (ToolsCallEvent (ToolsCallEventData evDat)) = do
    $logDebugS DM._LOGTAG "Run ToolsCallEvent called."

    go evDat $ evDat^.DM.paramsMcpToolsCallRequestData^.DM.nameMcpToolsCallRequestDataParams

    return noStateTransition
    
    where
      go :: DM.McpToolsCallRequestData -> String -> AppContext ()
      go dat "pty-connect"   = ptyConnectCommand dat
      go dat "pty-terminate" = ptyTerminateCommand dat
      go dat "pty-message"   = ptyMessageCommand dat
      go dat "pty-bash"      = ptyConnectCommand dat
      go dat "pty-ssh"       = ptyConnectCommand dat
      go dat "pty-cabal"     = ptyConnectCommand dat
      go dat "pty-stack"     = ptyConnectCommand dat
      go dat "pty-ghci"      = ptyConnectCommand dat
      go dat _ = cmdRunCommand dat


-- |
--
ptyConnectCommand :: DM.McpToolsCallRequestData -> AppContext ()
ptyConnectCommand dat = do
  resQ <- view DM.responseQueueDomainData <$> lift ask
  let cmdDat = DM.PtyConnectCommandData {
                DM._namePtyConnectCommandData      = dat^.DM.paramsMcpToolsCallRequestData^.DM.nameMcpToolsCallRequestDataParams
              , DM._argumentsPtyConnectCommandData = dat^.DM.paramsMcpToolsCallRequestData^.DM.argumentsMcpToolsCallRequestDataParams
              , DM._callbackPtyConnectCommandData  = callback dat resQ
              }

  cmdQ <- view DM.commandQueueDomainData <$> lift ask
  liftIO $ STM.atomically $ STM.writeTQueue cmdQ $ DM.PtyConnectCommand cmdDat


-- |
--
ptyTerminateCommand :: DM.McpToolsCallRequestData -> AppContext ()
ptyTerminateCommand dat = do
  resQ <- view DM.responseQueueDomainData <$> lift ask
  let cmdDat = DM.PtyTerminateCommandData {
                DM._namePtyTerminateCommandData      = dat^.DM.paramsMcpToolsCallRequestData^.DM.nameMcpToolsCallRequestDataParams
              , DM._argumentsPtyTerminateCommandData = dat^.DM.paramsMcpToolsCallRequestData^.DM.argumentsMcpToolsCallRequestDataParams
              , DM._callbackPtyTerminateCommandData  = callback dat resQ
              }

  cmdQ <- view DM.commandQueueDomainData <$> lift ask
  liftIO $ STM.atomically $ STM.writeTQueue cmdQ $ DM.PtyTerminateCommand cmdDat


-- |
--
ptyMessageCommand :: DM.McpToolsCallRequestData -> AppContext ()
ptyMessageCommand dat = do
  resQ <- view DM.responseQueueDomainData <$> lift ask
  let cmdDat = DM.PtyMessageCommandData {
                DM._namePtyMessageCommandData      = dat^.DM.paramsMcpToolsCallRequestData^.DM.nameMcpToolsCallRequestDataParams
              , DM._argumentsPtyMessageCommandData = dat^.DM.paramsMcpToolsCallRequestData^.DM.argumentsMcpToolsCallRequestDataParams
              , DM._callbackPtyMessageCommandData  = callback dat resQ
              }

  cmdQ <- view DM.commandQueueDomainData <$> lift ask
  liftIO $ STM.atomically $ STM.writeTQueue cmdQ $ DM.PtyMessageCommand cmdDat


-- |
--
callback :: DM.McpToolsCallRequestData -> STM.TQueue DM.McpResponse -> DM.SystemCommandCallback ()
callback evDat resQ code outStr errStr = do
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


-- |
--
cmdRunCommand :: DM.McpToolsCallRequestData -> AppContext ()
cmdRunCommand dat = do
  {-
  resQ <- view DM.responseQueueDomainData <$> lift ask
  let cmdDat2 = DM.SystemCommandData {
                DM._nameSystemCommandData = dat^.DM.paramsMcpToolsCallRequestData^.DM.nameMcpToolsCallRequestDataParams
              , DM._argumentsSystemCommandData = dat^.DM.paramsMcpToolsCallRequestData^.DM.argumentsMcpToolsCallRequestDataParams
              , DM._callbackSystemCommandData = callback dat resQ
              }

  cmdQ2 <- view DM.commandQueueDomainData <$> lift ask
  -}
  -- liftIO $ STM.atomically $ STM.writeTQueue cmdQ2 $ DM.SystemCommand cmdDat2

  let cmdDat = DM.DefaultCmdRunCommandData {
                DM._jsonrpcDefaultCmdRunCommandData   = dat^.DM.jsonrpcMcpToolsCallRequestData
              , DM._nameDefaultCmdRunCommandData      = dat^.DM.paramsMcpToolsCallRequestData^.DM.nameMcpToolsCallRequestDataParams
              , DM._argumentsDefaultCmdRunCommandData = dat^.DM.paramsMcpToolsCallRequestData^.DM.argumentsMcpToolsCallRequestDataParams
              }

  cmdQ <- view DM.cmdRunQueueDomainData <$> lift ask
  liftIO $ STM.atomically $ STM.writeTQueue cmdQ $ DM.DefaultCmdRunCommand cmdDat
