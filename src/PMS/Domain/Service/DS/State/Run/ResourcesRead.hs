{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module PMS.Domain.Service.DS.State.Run.ResourcesRead where


import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Logger
import Control.Lens
import Control.Monad.Reader
import qualified Control.Concurrent.STM as STM
import qualified Data.Text as T
import Data.Aeson
import Control.Monad.Except
import Data.Default
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Network.URI
import qualified Data.ByteString.Lazy as BL

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DM.Constant as DM

import PMS.Domain.Service.DM.Type
import qualified PMS.Domain.Service.DS.Utility as U


-- |
--
instance IStateActivity RunStateData ResourcesReadEventData where
  action _ (ResourcesReadEvent (ResourcesReadEventData evDat)) = flip catchError errHdl $ do
    $logDebugS DM._LOGTAG $ T.pack $ "Run ResourcesReadEvent called." ++ show evDat

    let params = evDat^.DM.paramsMcpResourcesReadRequestData
    uri <- getURI $ params^.DM.uriMcpResourcesReadRequestDataParams

    cont <- getUriContents uri

    response $ TL.unpack $ TLE.decodeUtf8 cont
   
    return noStateTransition

    where
      errHdl :: String -> AppContext (Maybe StateTransition)
      errHdl msg = do
        $logErrorS DM._LOGTAG $ T.pack $ "ResourcesReadEventData: exception occurred. " ++ msg
        response msg
        return noStateTransition

      getURI :: String -> AppContext URI
      getURI uriStr = case parseURI uriStr of
        Nothing   -> throwError "Invalid URI, parse fail."
        Just uri  -> return uri

      response :: String -> AppContext ()
      response cont = do
        let txtDat = def {DM._textTextResourceContents = cont} 
            content = encode txtDat
            result = DM.McpResourcesReadResponseResult [DM.RawJsonByteString content]
            jsonRpc = evDat^.DM.jsonrpcMcpResourcesReadRequestData
            resDat = DM.McpResourcesReadResponseData jsonRpc result
            res = DM.McpResourcesReadResponse resDat

        $logDebugS DM._LOGTAG $ T.pack $ show res

        queue <- view DM.responseQueueDomainData <$> lift ask
        liftIO $ STM.atomically $ STM.writeTQueue queue res
  
-- |
--
getUriContents :: URI -> AppContext BL.ByteString
getUriContents uri = case uriScheme uri of
  "file:" -> handleFileURI uri
  x       -> throwError $ "Unsupported scheme" ++ show x

-- |
--
handleFileURI :: URI -> AppContext BL.ByteString
handleFileURI uri = do
  resourcesDir <- view DM.resourcesDirDomainData <$> lift ask
  let resourcesFile = uriPath uri
      resourcesFilePath = resourcesDir ++ resourcesFile
      
  $logDebugS DM._LOGTAG $ T.pack $ "resourcesDir : " ++ resourcesDir
  $logDebugS DM._LOGTAG $ T.pack $ "resourcesFile : " ++ resourcesFile
  $logDebugS DM._LOGTAG $ T.pack $ "resourcesFilePath : " ++ resourcesFilePath

  U.readFile resourcesFilePath
