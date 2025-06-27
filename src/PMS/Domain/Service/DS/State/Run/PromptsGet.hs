{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module PMS.Domain.Service.DS.State.Run.PromptsGet where


import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Logger
import Control.Lens
import Control.Monad.Reader
import qualified Control.Concurrent.STM as STM
import qualified Data.Text as T
import Data.Aeson
import Control.Monad.Except
import System.FilePath
import Data.Default
import Text.Mustache
import qualified Data.Aeson as Aeson
import Data.Bifunctor

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DM.Constant as DM

import PMS.Domain.Service.DM.Type


-- |
--
instance IStateActivity RunStateData PromptsGetEventData where
  action _ (PromptsGetEvent (PromptsGetEventData evDat)) = flip catchError errHdl $ do
    $logDebugS DM._LOGTAG $ T.pack $ "Run PromptsGetEvent called." ++ show evDat

    let params = evDat^.DM.paramsMcpPromptsGetRequestData
        name = params^.DM.nameMcpPromptsGetRequestDataParams
        argsBS = DM.unRawJsonByteString $ params^.DM.argumentsMcpPromptsGetRequestDataParams
    
    promptsDir <- view DM.promptsDirDomainData <$> lift ask
    let promptsFile = name ++ ".md"
        promptsFilePath = promptsDir </> promptsFile
    
    argsDat <- liftEither $ eitherDecode $ argsBS
    let args = unJsonObjectMap argsDat
        
    $logDebugS DM._LOGTAG $ T.pack $ "promptsFile : " ++ promptsFile
    $logDebugS DM._LOGTAG $ T.pack $ "promptsFilePath : " ++ promptsFilePath
    $logDebugS DM._LOGTAG $ T.pack $ "arguments : " ++ show args

    tmplParams <- liftEither $ eitherDecode $ argsBS
    tmplTmp <- liftIO $ automaticCompile [promptsDir] promptsFile
    tmpl <- liftEither $ first show tmplTmp
    let rendered = substitute tmpl (tmplParams :: Aeson.Value)
        cont = T.unpack rendered
    
    -- cont <- U.readFile promptsFilePath
    -- response $ TL.unpack $ TLE.decodeUtf8 cont

    response cont
    
    return noStateTransition

    where
      errHdl :: String -> AppContext (Maybe StateTransition)
      errHdl msg = do
        $logErrorS DM._LOGTAG $ T.pack $ "PromptsGetEventData: exception occurred. " ++ msg
        response msg
        return noStateTransition

      response :: String -> AppContext ()
      response cont = do
        let role = "assistant"
            txtDat = def {DM._textMcpTextContent = cont} 
            content = encode txtDat
            msg = DM.McpPromptsGetResponseResultPromptMessage role (DM.RawJsonByteString content)
            result = DM.McpPromptsGetResponseResult [msg]
            jsonRpc = evDat^.DM.jsonrpcMcpPromptsGetRequestData
            resDat = DM.McpPromptsGetResponseData jsonRpc result
            res = DM.McpPromptsGetResponse resDat

        $logDebugS DM._LOGTAG $ T.pack $ show res

        queue <- view DM.responseQueueDomainData <$> lift ask
        liftIO $ STM.atomically $ STM.writeTQueue queue res
  