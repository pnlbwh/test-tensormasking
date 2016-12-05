module Intrust
    (path
    )
    where

import Text.Printf
import Development.Shake
import Development.Shake.FilePath

path :: String -> String -> FilePath
path var caseid = let base = "/data/pnl/INTRuST/" in
  case var of
    "dwied" -> printf (base </> "%s/diff/%s-dwi-Ed.nhdr") caseid caseid
    "dwimask" -> printf (base </> "%s/diff/%s-tensor-mask.nhdr") caseid caseid
    "dwiharm" -> printf (base </> "Harmonization-20160728/%s_hmz_iter1.nhdr") caseid
    "fsindwi" -> printf (base </> "%s/diff/%s.fsindwi.nrrd") caseid caseid
    "ukf_dwiharm_cpp" -> printf (base </> "harmonized-newquery/%s/diff/%s.ukf_2T.vtk.gz") caseid caseid
    _ -> error "Add this var to Intrust.hs"
  {-where base = "/Users/ryan/partners/data/pnl/INTRuST/"-}
