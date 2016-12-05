{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Pipeline.Structural
  ( StructuralType (..)
  , Structural (..)
  , rules
  ) where

import           Data.Maybe      (fromMaybe)
import qualified Paths
import           Shake.BuildNode

type CaseId = String

data StructuralType = T1w | T2w
                    deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

newtype Structural = Structural (StructuralType, CaseId)
                    deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode Structural where
  path (Structural (T1w, caseid)) = fromMaybe (error "Set 't1' path in Paths.hs") $ Paths.t1 caseid
  path (Structural (T2w, caseid)) = fromMaybe (error "Set 't2' path in Paths.hs") $ Paths.t2 caseid


rules = rule (buildNode :: Structural -> Maybe (Action [Double]))
