{-# LANGUAGE FlexibleInstances #-}
module HCPConfig
  ( outdir
  -- Normalized
  , sourceDwi_path
  , normalizedDwi_path
  , b0sPairsYaml_path
  , meanB0_path
  -- Preprocessing
  , acqParams_path
  , posNegVol_path
  , index_path
  , series_path
  , b0s_path
  -- Topup
  , hiFiB0_path
  , topupOutputPrefix_path
  , applyTopupOutputPrefix_path
  , topupConfig_path
  ) where

import           Shake.BuildKey
import           HCP.Types
import           Text.Printf

-----------------------------------------------------------------------
-- Input Paths

sourceDwi_path :: PhaseOrientation -> Int -> CaseId -> FilePath
sourceDwi_path Pos num caseid = printf "src/%s.dwiPA%d.nii.gz" caseid num
sourceDwi_path Neg num caseid = printf "src/%s.dwiAP%d.nii.gz" caseid num

-----------------------------------------------------------------------
-- Output Directory

outdir :: FilePath
outdir = "_data"

-----------------------------------------------------------------------
-- Topup
hiFiB0_path caseid = outdir </> caseid </> "hcp/2_Topup" </> "hifib0.nii.gz"
applyTopupOutputPrefix_path caseid = outdir </> caseid </> "hcp/2_Topup" </> "topup_Pos_Neg_b0"
topupOutputPrefix_path caseid = outdir </> caseid </> "hcp/2_Topup" </> "topup_Pos_Neg_b0"
topupConfig_path = "src/hcp/b02b0.cnf"

-----------------------------------------------------------------------
-- Preprocessing Paths

acqParams_path caseid = outdir </> caseid </> "hcp/1_Preprocessing" </> "acqparams.txt"
posNegVol_path caseid = outdir </> caseid </> "hcp/1_Preprocessing" </> "PosNeg.nii.gz"
index_path caseid = outdir </> caseid </> "hcp/1_Preprocessing" </> "index.txt"
series_path orientation caseid = outdir </> caseid </> "hcp/1_Preprocessing"
  </> (show orientation) ++ "_SeriesVolNum.txt"
b0s_path orientation caseid = outdir </> caseid </> "hcp/1_Preprocessing"
  </> (show orientation) ++ "_b0s.nii.gz"

-----------------------------------------------------------------------
-- Normalization Paths

normalizedDwi_path :: PhaseOrientation -> Int -> CaseId -> FilePath
normalizedDwi_path phasedir num caseid =
  -- [qc|{outdir}/{caseid}/hcp/0_normalized/{phasedir}-{num}.nii.gz|]
    foldr (</>) ""
    [outdir
    ,caseid
    ,"hcp/0_normalized"
    ,printf "%s-%i.nii.gz" (show phasedir) num
    ]

b0sPairsYaml_path caseid =
  -- [qq|{outdir}/$caseid/hcp/0_normalized/dwipairs.yaml|]
    foldr (</>) ""
    [outdir
    ,caseid
    ,"hcp/0_normalized/b0sPairs.yaml"]

meanB0_path caseid =
  -- [qc|{outdir}/{caseid}/hcp/0_normalized/Pos-1-meanb0|]
    foldr (</>) ""
    [outdir
    ,caseid
    ,"hcp/0_normalized/Pos-1-meanb0.txt"]