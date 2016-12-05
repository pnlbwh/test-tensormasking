{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Pipeline.HCP.Types
  ( PhaseEncoding (..)
  , PhaseOrientation (..)
  , CaseId
  -- , DwiScan (..)
  , EchoSpacing
  , PhaseLength
  ) where

import           Shake.BuildNode

type CaseId = String
type EchoSpacing = Float
type PhaseLength = Int

data PhaseEncoding = RL | LR | PA | AP
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

data PhaseOrientation = Pos | Neg
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)
-- data DwiScan = NormalizedDwiScan
--              | SourceDwiScan
--         deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)
