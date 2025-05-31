{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module PMS.Domain.Service.App.ControlSpec (spec) where

import Test.Hspec
import Control.Concurrent.Async
import qualified Control.Concurrent.STM as STM
import Control.Lens
import Data.Default

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Service.App.Control as SUT
import qualified PMS.Domain.Service.DM.Type as SUT


-- |
--
data SpecContext = SpecContext {
                   _domainDataSpecContext :: DM.DomainData
                 , _appDataSpecContext :: SUT.AppStateW
                 }

makeLenses ''SpecContext

defaultSpecContext :: IO SpecContext
defaultSpecContext = do
  domDat <- DM.defaultDomainData
  let appDat = SUT.AppStateW SUT.StartState
  return SpecContext {
           _domainDataSpecContext = domDat
         , _appDataSpecContext    = appDat
         }

-- |
--
spec :: Spec
spec = do
  runIO $ putStrLn "Start Spec."
  beforeAll setUpOnce $ 
    afterAll tearDownOnce . 
      beforeWith setUp . 
        after tearDown $ run


-- |
--
setUpOnce :: IO SpecContext
setUpOnce = do
  putStrLn "[INFO] EXECUTED ONLY ONCE BEFORE ALL TESTS START."
  defaultSpecContext

-- |
--
tearDownOnce :: SpecContext -> IO ()
tearDownOnce _ = do
  putStrLn "[INFO] EXECUTED ONLY ONCE AFTER ALL TESTS FINISH."

-- |
--
setUp :: SpecContext -> IO SpecContext
setUp ctx = do
  putStrLn "[INFO] EXECUTED BEFORE EACH TEST STARTS."

  domDat <- DM.defaultDomainData
  return ctx {  _domainDataSpecContext = domDat
              , _appDataSpecContext    = SUT.AppStateW SUT.StartState
              }

-- |
--
tearDown :: SpecContext -> IO ()
tearDown _ = do
  putStrLn "[INFO] EXECUTED AFTER EACH TEST FINISHES."

-- |
--
run :: SpecWith SpecContext
run = do
  describe "runApp" $ do
    context "when AppData default" $ do
      it "should be run" $ \ctx -> do 
        putStrLn "[INFO] EXECUTING THE FIRST TEST."

        let domDat = ctx^.domainDataSpecContext
            appDat = ctx^.appDataSpecContext
            reqQ   = domDat^.DM.requestQueueDomainData
            resQ   = domDat^.DM.responseQueueDomainData
            args   = DM.McpInitializeRequest def
            expect = 7
            
        thId <- async $ SUT.runWithAppData appDat domDat

        STM.atomically $ STM.writeTQueue reqQ args

        actual <- STM.atomically $ STM.readTQueue resQ
        putStrLn $ show actual
        -- actual `shouldBe` expect

        cancel thId

