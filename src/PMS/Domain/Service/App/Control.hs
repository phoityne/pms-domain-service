{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}


module PMS.Domain.Service.App.Control where

import System.IO
import qualified Control.Exception.Safe as E
import System.Log.FastLogger

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DS.Utility as DM

import PMS.Domain.Service.DM.Constant
import PMS.Domain.Service.DM.Type
import PMS.Domain.Service.DS.Core
import PMS.Domain.Service.DS.State.Start()
import PMS.Domain.Service.DS.State.Run()
import PMS.Domain.Service.DS.State.Stop()

import PMS.Domain.Service.DS.Utility


-- |
--
run :: DM.DomainContext ()
run dat = do
  hPutStrLn stderr "[INFO] PMS.Domain.Service.App.Control.run called."

  let appDat = AppStateW StartState
  runWithAppData appDat dat

-- |
--
runWithAppData :: AppStateW -> DM.DomainContext ()
runWithAppData appDat domDat = do
  logDat <- DM.createLogger domDat _LOG_FILE_NAME
  runWithLogger logDat appDat domDat

-- |
--
runWithLogger :: (TimedFastLogger, IO ()) -> AppStateW -> DM.DomainContext ()
runWithLogger (logger, finalizeLogger) appDat domDat = 
  flip E.catchAny exception
    $ flip E.finally finalize
    $ runAppState domDat appDat logger app
    >>= \case
      Right (x, _) -> return x
      Left  e -> errorEnd e

  where
    finalize = do
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      hPutStrLn stderr "[INFO] PMS.Domain.Service.App.Control.run finalize called."
      finalizeLogger
      hPutStrLn stderr "-----------------------------------------------------------------------------"

    exception e = do
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      hPutStrLn stderr "[ERROR] PMS.Domain.Service.App.Control.run exception occurred."
      hPutStrLn stderr $ show e
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      E.throwIO e

    errorEnd e = do
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      hPutStrLn stderr "[ERROR] PMS.Domain.Service.App.Control.run end with error."
      hPutStrLn stderr $ show e
      hPutStrLn stderr "-----------------------------------------------------------------------------"
