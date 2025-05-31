{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PMS.Domain.Service.DS.State.Start.Terminate where

import PMS.Domain.Service.DM.Type


-- |
--
instance IStateActivity StartStateData TerminateEventData
  -- @see default implementation in Type module.

