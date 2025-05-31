{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PMS.Domain.Service.DS.State.Start.Launch where

import PMS.Domain.Service.DM.Type


-- |
--
instance IStateActivity StartStateData LaunchEventData
  -- @see default implementation in Type module.

