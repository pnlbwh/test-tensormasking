{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Pipeline.FreeSurfer
  ( rules
  , FreeSurfer (..)
  ) where

import           Data.Maybe
import qualified FreeSurfer              (runWithMask)
import           Shake.BuildNode
import qualified Paths                   (freeSurfer, t1)
import           Pipeline.Structural     hiding (rules)
import           Pipeline.StructuralMask hiding (rules)

type CaseId = String

newtype FreeSurfer = FreeSurfer (StructuralMaskType, CaseId)
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode FreeSurfer where
  -- Picking wmparc.mgz as representative of freesurfer subject directory
  path (FreeSurfer (_, caseid))
    = Paths.freeSurfer caseid </> "mri" </> "wmparc.mgz"

  build (FreeSurfer (strctMaskType, caseid)) = Just $ do
    let strct = Structural (T1w, caseid)
    let mask = StructuralMask (strctMaskType, T1w, caseid)
    need mask
    need strct
    FreeSurfer.runWithMask
      [5,3,0]
      (path mask)
      (path strct)
      (Paths.freeSurfer caseid)

rules = rule (buildNode :: FreeSurfer -> Maybe (Action [Double]))
