{-# LANGUAGE OverloadedStrings #-}

module PMS.Domain.Service.DS.Utility where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Except
import Control.Monad.Reader
import System.Log.FastLogger
import qualified Control.Exception.Safe as E
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import System.Directory

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DS.Utility as DM
import PMS.Domain.Service.DM.Type


-- |
--
changeTo :: AppStateW -> AppStateContext ()
changeTo nextSt = do
  curSt <- get
  _ <- actionSW curSt (EventW ExitEvent)

  let req = EventW EntryEvent
  _ <- actionSW nextSt req

  modify (\_ -> nextSt)


-- |
--
runAppState :: DM.DomainData -> AppStateW -> TimedFastLogger -> AppStateContext a -> IO (Either DM.ErrorData (a, AppStateW))
runAppState domDat st logger ctx =
  DM.runFastLoggerT domDat logger
    $ runExceptT
    $ flip runReaderT domDat
    $ runStateT ctx st

-- |
--
liftIOE :: IO a -> AppStateContext a
liftIOE f = liftIO (go f) >>= liftEither
  where
    go :: IO b -> IO (Either String b)
    go x = E.catchAny (Right <$> x) errHdl

    errHdl :: E.SomeException -> IO (Either String a)
    errHdl = return . Left . show

-- |
--
readFile :: FilePath -> AppContext BL.ByteString
readFile path = isFileExists path
            >>= isReadable
            >>= go
  where
    go f = liftIOE $ do
      cont <- T.readFile f
      return $ BL.fromStrict $ TE.encodeUtf8 cont

-- |
--
isFileExists :: FilePath -> AppContext FilePath
isFileExists f = liftIOE (doesFileExist f) >>= \case
  True  -> return f
  False -> throwError $ "invalid file. not exists." ++ f

-- |
--
isReadable :: FilePath -> AppContext FilePath
isReadable f = liftIOE (readable <$> getPermissions f) >>= \case
  True  -> return f
  False -> throwError $ "invalid file. not readable." ++ f
