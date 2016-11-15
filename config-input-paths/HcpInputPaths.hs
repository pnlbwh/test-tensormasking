module HcpInputPaths
  ( sourceDwi_path
  , topupConfig_path
  ) where

import           Shake.BuildKey
import           HCP.Types
import           Text.Printf

-----------------------------------------------------------------------
-- Input Paths

sourceDwi_path :: PhaseOrientation -> Int -> CaseId -> FilePath
sourceDwi_path Pos num caseid = printf "in/%s.dwiPA%d.nii.gz" caseid num
sourceDwi_path Neg num caseid = printf "in/%s.dwiAP%d.nii.gz" caseid num

topupConfig_path = "config/hcp_b02b0.cnf"