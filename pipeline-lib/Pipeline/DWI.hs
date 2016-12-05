{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Pipeline.DWI
  ( DwiType (..)
  , Dwi (..)
  , rules
  ) where

import           Data.List                  (intercalate)
import           Data.Maybe                 (fromMaybe)
import           FSL
import           Paths
import qualified Pipeline.HCP
import           Pipeline.HCP.B0sPair
import           Pipeline.HCP.Eddy          hiding (rules)
import qualified Pipeline.HCP.Normalize     as N
import qualified Pipeline.HCP.Preprocessing as P
import           Pipeline.HCP.Types
import           Pipeline.Util              (showKey)
import           Shake.BuildNode
import           System.Directory           as IO (renameFile)

data DwiType = DwiGiven
             | DwiHcp [Int]
             deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

newtype Dwi = Dwi (DwiType, CaseId)
             deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance FslDwi Dwi where
    nifti n@(Dwi (_, caseid)) = outdir </> caseid </> showKey n <.> "nii.gz"

instance BuildNode Dwi where
  paths n@(Dwi (DwiHcp _, caseid)) = [nifti n, bval n, bvec n]
  paths (Dwi (DwiGiven, caseid)) = fromMaybe (error "Set 'dwi' in Paths.hs") $
                                        fmap (:[]) $ Paths.dwi caseid
  -- TODO assumes nrrd
  -- paths n@(Dwi (_, caseid)) = [outdir </> caseid </> showKey n <.> "nrrd"]

  build (Dwi (DwiGiven, _)) = Nothing

  -- build n@(Dwi (DwiXc, caseid)) = Just $ do
  --   need $ Dwi (DwiGiven, caseid)
  --   command_ [] "config/axis_align_nrrd.py" ["-i", path $ Dwi (DwiGiven, caseid)
  --                                           ,"-o", path n]
  --   command_ [] "config/center.py" ["-i", path n
  --                                  ,"-o", path n]

  build n@(Dwi (DwiHcp indices, caseid)) = Just $ do
    need $ EddyUnwarpedImages (indices, caseid)
    needs [P.Series orient indices caseid | orient <- [Pos, Neg]]
    needs [N.Dwi (N.DwiJoined orient indices, caseid) | orient <- [Pos, Neg]]
    b0spairs <- N.getB0sPairs caseid indices
    let numPos = show $ sum $ map (_size._pos) b0spairs
        numNeg = show $ sum $ map (_size._neg) b0spairs
    withTempFile $ \eddypos ->
      withTempFile $ \eddyneg -> do
        let outdir = pathDir n
        command_ [] "fslroi" [path $ EddyUnwarpedImages (indices, caseid)
                             , eddypos
                             , "0"
                             , numPos]
        command_ [] "fslroi" [path $ EddyUnwarpedImages (indices, caseid)
                             , eddyneg
                             , numPos
                             , numNeg]
        command_ [] "eddy_combine" [ eddypos
                                   , bval $ N.Dwi (N.DwiJoined Pos indices, caseid)
                                   , bvec $ N.Dwi (N.DwiJoined Pos indices, caseid)
                                   , path (P.Series Pos indices caseid)
                                   , eddyneg
                                   , bval $ N.Dwi (N.DwiJoined Neg indices, caseid)
                                   , bvec $ N.Dwi (N.DwiJoined Neg indices, caseid)
                                   , path (P.Series Neg indices caseid)
                                   , (pathDir n), "1"
                                   ]
        -- Remove negative intensity values (caused by spline interpolation) from final data
        liftIO $ IO.renameFile (outdir </> "data.nii.gz") (nifti n)
        command_ [] "fslmaths" [nifti n, "-thr", "0", nifti n]
        liftIO $ IO.renameFile (outdir </> "bvecs") (bvec n)
        liftIO $ IO.renameFile (outdir </> "bvals") (bval n)


-- DWIConvert --conversionMode FSLToNrrd --inputBVectors data-1.bvec --inputBValues data-1.bval --fslNIFTIFile data-1.nii.gz -o data-1.nrrd

rules = rule (buildNode :: Dwi -> Maybe (Action [Double]))
