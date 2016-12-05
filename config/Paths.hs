module Paths
  (outdir
  ,t1
  ,t2
  ,t1mask
  ,t2mask
  ,dwi
  ,dwimask
  ,fsInDwiDir
  ,dwiHcp
  ,t1mabs
  ,t2mabs
  ,t1rigidmabs
  ,t2rigidmabs
  ,t1rigidmask
  ,t2rigidmask
  ,freeSurfer
  -- ,freeSurferGiven
  ,hcpdir
  -- software
  ,ukfTractographyExePrefix
  ,ukfTractographyDir
  ,tractQuerierPrefix
  ,antsPrefix
  )
  where

import           Data.List                  (intercalate)
import           Development.Shake.FilePath (FilePath, (<.>), (</>))
import           Pipeline.HCP.Types         (PhaseOrientation (..))
import           Text.Printf

--------------------------------------------------------------------------------
-- Source (ungenerated) data paths - modify these
-- TODO make optionals a maybe type

t1 caseid = Just $ "in" </> caseid </> caseid ++ "_3T_T1w_MPR1.nii.gz"
t2 caseid = Just $ "in" </> caseid </> caseid ++ "_3T_T2w_SPC1.nii.gz"
t1mask caseid = Just $ "in" </> caseid </> caseid ++ "-mabsT1Mask.nii.gz"
t2mask caseid = Nothing
dwi caseid = Nothing
dwimask caseid = Nothing
dwiHcp Pos num caseid = Just $ "in" </> caseid </> (show num) ++ "PA" <.> "nii.gz"
dwiHcp Neg num caseid = Just $ "in" </> caseid </> (show num) ++ "AP" <.> "nii.gz"
-- freeSurferGiven caseid = Just $ "in" </> caseid </> caseid ++ "-freesurfer"

--------------------------------------------------------------------------------
-- Generated Data Path (defaults should be fine)

outdir = "_data"
t1mabs caseid = outdir </> caseid </> caseid ++ "-" ++ "mabsT1Mask.nii.gz"
t2mabs caseid = outdir </> caseid </> caseid ++ "-" ++ "mabsT2Mask.nii.gz"
t1rigidmabs caseid = outdir </> caseid </> caseid ++ "-" ++ "rigid-mabs.nii.gz"
t2rigidmabs caseid = outdir </> caseid </> caseid ++ "-" ++ "rigid-mabs.nii.gz"
t1rigidmask caseid = outdir </> caseid </> caseid ++ "-" ++ "rigid-mask.nii.gz"
t2rigidmask caseid = outdir </> caseid </> caseid ++ "-" ++ "rigid-mask.nii.gz"
freeSurfer caseid = outdir </> caseid </> caseid ++ "-" ++ "freesurfer"
fsInDwiDir caseid = outdir </> caseid
ukfTractographyExePrefix = outdir </> "UKFTractography"
ukfTractographyDir caseid = outdir </> caseid
tractQuerierPrefix = outdir </> "tract_querier"
antsPrefix = outdir </> "ANTs"

showIndices :: [Int] -> String
showIndices = intercalate "-" . map show

hcpdir :: String -> FilePath -> FilePath
hcpdir caseid stage = outdir </> caseid </> "hcp" </> stage
