{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module PMS.Domain.Service.DM.TH (
    funcTH_transit
  , instanceTH_IAppState
  ) where

import qualified GHC.Base
import qualified GHC.Show
import qualified Control.Monad.Fail
import Control.Monad.Trans.State.Lazy
import Language.Haskell.TH
import Control.Monad
import qualified Data.Text as T

import PMS.Domain.Service.DM.Type
import PMS.Domain.Service.DS.Utility


-- |
--
{-

> stack ghci --ghci-options=-XTemplateHaskell --ghci-options=-XQuasiQuotes
*Main> :m +Language.Haskell.TH

:{
 runQ [d|
   instance IAppState StartStateData where
     actionS s (EventW r@EntryEvent{})      = action s r
   |]
:}

[InstanceD Nothing [] (AppT (ConT Type.IAppState) (ConT Type.StartState))
  [FunD Type.doActivity
    [Clause [VarP s_8,ConP Type.EventW [AsP r_9 (RecP Type.InitEvent [])]] (NormalB (AppE (AppE (VarE Type.action) (VarE s_8)) (VarE r_9))) [],
     Clause [VarP s_10,ConP Type.EventW [AsP r_11 (RecP Type.TerminateEvent [])]] (NormalB (AppE (AppE (VarE Type.action) (VarE s_10)) (VarE r_11))) []
    ]
  ]
]

[InstanceD Nothing [] (AppT (ConT Type.IAppState) (ConT Type.StartStateData))
  [FunD Type.actionS 
    [Clause [VarP s_0,ConP Type.EventW [] [AsP r_1 (RecP Type.EntryEvent [])]] (NormalB (AppE (AppE (VarE Type.action) (VarE s_0)) (VarE r_1))) []
    ]
  ]
]


:{
 runQ [d|
   data Event r where
     InitEvent :: Event InitEvent
   |]
:}

[DataD [] Event_12 [PlainTV r_14] Nothing [GadtC [InitEvent_13] [] (AppT (ConT Event_12) (ConT Type.InitEvent))] []]

-}

instanceTH_IAppState :: Name -> Q [Dec]
instanceTH_IAppState stName = do
  ns <- getGadtsContNames ''Event
  clauseList <- mapM go ns
  return $ [InstanceD Nothing [] (AppT (ConT ''IAppState) (ConT stName)) [FunD 'actionS clauseList]]

  where
    -- |
    --
    go n = do
      s <- newName "s"
      r <- newName "r"
      return $ Clause [VarP s, ConP 'EventW [] [AsP r (RecP n [])]] (NormalB (AppE (AppE (VarE 'action) (VarE s)) (VarE r))) []

    -- |
    --
    getGadtsContNames :: Name -> Q [Name]
    getGadtsContNames n = reify n >>= \case
      TyConI (DataD _ _ _ _ cs _) -> mapM go' cs
      x -> fail $ "[ERROR] can not get data constructor. " ++ show x

      where
        go' (GadtC [name] _ _) = return name
        go' x = fail $ "[ERROR] can not get gadts data constructor. " ++ show x


-- |
--
{-

> stack ghci --ghci-options=-XTemplateHaskell --ghci-options=-XQuasiQuotes --ghci-options=-XLambdaCase
*Main> :m +Language.Haskell.TH

:{
 runQ [d|
    transit :: StateTransition -> AppContext ()
    transit StartToRun = get >>= \case
      AppStateW StartState -> changeTo $ AppStateW RunState
      AppStateW x -> fail $ "invalid state transition. trans:" ++ show StartToRun ++ ", curSt:" ++ show x
    transit RunToStop = get >>= \case
      AppStateW RunState -> changeTo $ AppStateW StopState
      AppStateW x -> fail $ "invalid state transition. trans:" ++ show StartToRun ++ ", curSt:" ++ show x
   |]
:}

[SigD transit_0 (AppT (AppT ArrowT (ConT Type.StateTransition)) (AppT (ConT Type.AppStateContext) (TupleT 0))),
  FunD transit_0 [
    Clause [ConP Type.StartToRun []]
     (NormalB (InfixE (Just (UnboundVarE get)) (VarE GHC.Base.>>=) (Just (LamCaseE [
        Match (ConP Type.AppStateW [ConP Type.StartState []]) (NormalB (InfixE (Just (UnboundVarE changeTo)) (VarE GHC.Base.$) (Just (AppE (ConE Type.AppStateW) (ConE Type.RunState))))) [],
        Match (ConP Type.AppStateW [VarP x_5])
          (NormalB (InfixE
            (Just (VarE GHC.Base.fail))
            (VarE GHC.Base.$)
            (Just (InfixE
              (Just (LitE (StringL "invalid state transition. trans:")))
                (VarE GHC.Base.++)
                (Just (InfixE
                  (Just (AppE (VarE GHC.Show.show) (ConE Type.StartToRun)))
                  (VarE GHC.Base.++)
                  (Just (InfixE
                    (Just (LitE (StringL ", curSt:")))
                      (VarE GHC.Base.++)
                      (Just (AppE (VarE GHC.Show.show) (VarE x_1)))
                  ))
                ))
            ))
          )) []
      ])))) [],
    Clause [
      ConP Type.RunToStop []] (NormalB (InfixE (Just (UnboundVarE get)) (VarE GHC.Base.>>=) (Just (LamCaseE [
        Match (ConP Type.AppStateW [ConP Type.RunState []]) (NormalB (InfixE (Just (UnboundVarE changeTo)) (VarE GHC.Base.$) (Just (AppE (ConE Type.AppStateW) (ConE Type.StopState))))) [],
        Match (VarP x_2) (NormalB (InfixE (Just (VarE GHC.Base.fail)) (VarE GHC.Base.$) (Just (InfixE (Just (LitE (StringL "invalid state transition. trans:"))) (VarE GHC.Base.++) (Just (InfixE (Just (AppE (VarE GHC.Show.show) (ConE Type.StartToRun))) (VarE GHC.Base.++) (Just (InfixE (Just (LitE (StringL ", curSt:"))) (VarE GHC.Base.++) (Just (AppE (VarE GHC.Show.show) (VarE x_2))))))))))) []])))) []]]
    ]
]

[SigD transit_2 (AppT (AppT ArrowT (ConT Type.StateTransition)) (AppT (ConT Type.AppContext) (TupleT 0))),
  FunD transit_2 [
    Clause [ConP Type.StartToRun [] []]
      (NormalB (InfixE (Just (VarE Control.Monad.Trans.State.Lazy.get)) (VarE GHC.Internal.Base.>>=) (Just (LamCaseE [
        Match (ConP Type.AppStateW [] [ConP Type.StartState [] []]) (NormalB (InfixE (Just (UnboundVarE changeTo)) (VarE GHC.Internal.Base.$) (Just (AppE (ConE Type.AppStateW) (ConE Type.RunState))))) [],
        Match (ConP Type.AppStateW [] [VarP x_3])
          (NormalB (InfixE 
            (Just (VarE GHC.Internal.Control.Monad.Fail.fail))
              (VarE GHC.Internal.Base.$) 
              (Just (InfixE
                (Just (LitE (StringL "invalid state transition. trans:")))
                  (VarE GHC.Internal.Base.++)
                  (Just (InfixE 
                    (Just (AppE (VarE GHC.Internal.Show.show) (ConE Type.StartToRun))) 
                    (VarE GHC.Internal.Base.++)
                    (Just (InfixE (Just (LitE (StringL ", curSt:")))
                      (VarE GHC.Internal.Base.++)
                      (Just (AppE (VarE GHC.Internal.Show.show) (VarE x_3)))
                    ))
                  ))
                ))
              )) []
        ])))) [],
    Clause [ConP Type.RunToStop [] []] (NormalB (InfixE (Just (VarE Control.Monad.Trans.State.Lazy.get)) (VarE GHC.Internal.Base.>>=) (Just (LamCaseE [
      Match (ConP Type.AppStateW [] [ConP Type.RunState [] []]) (NormalB (InfixE (Just (UnboundVarE changeTo)) (VarE GHC.Internal.Base.$) (Just (AppE (ConE Type.AppStateW) (ConE Type.StopState))))) [],
      Match (ConP Type.AppStateW [] [VarP x_4]) (NormalB (InfixE (Just (VarE GHC.Internal.Control.Monad.Fail.fail)) (VarE GHC.Internal.Base.$) (Just (InfixE (Just (LitE (StringL "invalid state transition. trans:"))) (VarE GHC.Internal.Base.++) (Just (InfixE (Just (AppE (VarE GHC.Internal.Show.show) (ConE Type.StartToRun))) (VarE GHC.Internal.Base.++) (Just (InfixE (Just (LitE (StringL ", curSt:"))) (VarE GHC.Internal.Base.++) (Just (AppE (VarE GHC.Internal.Show.show) (VarE x_4))))))))))) []])))) []
    ]
]


-}

funcTH_transit :: Q [Dec]
funcTH_transit = do
  fname <- newName "transit"
  cons <- getContNames ''StateTransition
  clauses <- mapM makeClaues cons

  return $ [SigD fname (AppT (AppT ArrowT (ConT ''StateTransition)) (AppT (ConT ''AppStateContext) (TupleT 0))),
            FunD fname clauses
           ]

  where
    -- |
    --
    getContNames :: Name -> Q [Name]
    getContNames n = reify n >>= \case
      TyConI (DataD _ _ _ _ cs _) -> mapM go cs
      x -> fail $ "[ERROR] can not get data constructor. " ++ show x

      where
        go (NormalC x _) = return x
        go x = fail $ "[ERROR] can not get data constructor. " ++ show x

    -- |
    --
    makeClaues :: Name -> Q Clause
    makeClaues n = do
      x <- newName "x"
      (curSt, nexSt) <- getStName n
      return $ Clause
        [ConP n [] []]
        (NormalB
          (InfixE (Just (UnboundVarE 'get)) (VarE '(GHC.Base.>>=))
            (Just (LamCaseE [
              Match (ConP 'AppStateW [] [ConP curSt [] []])
                (NormalB (InfixE (Just (UnboundVarE 'changeTo)) (VarE '(GHC.Base.$)) (Just (AppE (ConE 'AppStateW) (ConE nexSt))) )) [],
              Match (ConP 'AppStateW [] [VarP x])
                (NormalB (InfixE
                  (Just (VarE 'Control.Monad.Fail.fail))
                  (VarE '(GHC.Base.$))
                  (Just (InfixE
                    (Just (LitE (StringL "invalid state transition. trans:")))
                      (VarE '(GHC.Base.++))
                      (Just (InfixE
                        (Just (AppE (VarE 'GHC.Show.show) (ConE n)))
                        (VarE '(GHC.Base.++))
                        (Just (InfixE
                          (Just (LitE (StringL ", curSt:")))
                            (VarE '(GHC.Base.++))
                            (Just (AppE (VarE 'GHC.Show.show) (VarE x)))
                        ))
                      ))
                  ))
                )) []
            ]))
          )
        )
        []

    -- |
    --
    getStName :: Name -> Q (Name, Name)
    getStName n = do
      let modName = "PMS.Domain.Service.DM.Type."

      let stStrs = T.splitOn "To" $ T.replace modName "" $ T.pack $ show n
      when (2 /= length stStrs) $ fail $ "[ERROR] invalid StateTransition constructor. " ++ show n

      let curSt = mkName $ T.unpack $ modName `T.append` head stStrs `T.append` "State"
          nxtSt = mkName $ T.unpack $ modName `T.append` last stStrs `T.append` "State"

      return (curSt, nxtSt)
