{-# LANGUAGE FlexibleInstances #-}
module HcpOutputPaths
  ( outdir
  -- Normalized
  , normalizedDwi_path
  , b0sPairsYaml_path
  , meanB0_path
  -- Preprocessing
  , acqParams_path
  , posNegDwi_path
  , posDwi_path
  , negDwi_path
  , index_path
  , series_path
  , b0s_path
  -- Topup
  , noDifBrainMaskPrefix_path
  , hiFiB0_path
  , topupOutputPrefix_path
  -- Eddy
  , eddyUnwarpedImages_path
  -- PostEddy
  , dataDwi_path
  , dataBrainMaskPrefix_path
  ) where

import           HCP.Types       (CaseId, PhaseOrientation)
import           OutputDirectory (outdir)
import           Shake.BuildKey
import           Text.Printf     (printf)


-----------------------------------------------------------------------
-- PostEddy

dataDwi_path caseid = outdir </> caseid </> "hcp" </>
  "4_Data" </> "data.nii.gz"
dataBrainMaskPrefix_path caseid = outdir </> caseid </> "hcp" </>
  "4_Data" </> "nodif_brain"


-----------------------------------------------------------------------
-- Eddy

eddyUnwarpedImages_path caseid = outdir </> caseid </> "hcp" </>
  "3_Eddy" </> "eddy_unwarped_images.nii.gz"


-----------------------------------------------------------------------
-- Topup
noDifBrainMaskPrefix_path caseid = outdir </> caseid </> "hcp/2_Topup" </> "nodif_brain"
hiFiB0_path caseid = outdir </> caseid </> "hcp/2_Topup" </> "hifib0.nii.gz"
topupOutputPrefix_path caseid = outdir </> caseid </> "hcp/2_Topup" </> "topup_Pos_Neg_b0"

-----------------------------------------------------------------------
-- Preprocessing Paths

acqParams_path caseid = outdir </> caseid </> "hcp/1_Preprocessing" </> "acqparams.txt"
posNegDwi_path caseid = outdir </> caseid </> "hcp/1_Preprocessing" </> "PosNeg.nii.gz"
posDwi_path caseid = outdir </> caseid </> "hcp/1_Preprocessing" </> "Pos.nii.gz"
negDwi_path caseid = outdir </> caseid </> "hcp/1_Preprocessing" </> "Neg.nii.gz"
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
