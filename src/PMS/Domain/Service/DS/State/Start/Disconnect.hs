{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PMS.Domain.Service.DS.State.Start.Disconnect where

import PMS.Domain.Service.DM.Type

-- |
--
instance IStateActivity StartStateData DisconnectEventData
  -- @see default implementation in Type module.

