{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Pipeline.HCP.Eddy
  ( rules
  , EddyUnwarpedImages (..)
  ) where

import qualified FSL
import           qualified Paths             (hcpdir)
import qualified Pipeline.HCP.Preprocessing as Preprocessing
import qualified Pipeline.HCP.Normalize as N
import qualified Pipeline.HCP.Topup         as Topup
import           Pipeline.HCP.Types         (CaseId, PhaseOrientation (..))
import           Shake.BuildNode
import           Pipeline.Util              (showKey)

stage = "3_Eddy"

newtype EddyUnwarpedImages = EddyUnwarpedImages ([Int], CaseId)
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode EddyUnwarpedImages where
  path n@(EddyUnwarpedImages (indices, caseid)) = Paths.hcpdir caseid stage </>
    showKey n <.> "nii.gz"

  build out@(EddyUnwarpedImages (indices, caseid)) = Just $ do
    need $ N.Dwi (N.DwiJoinedAll indices, caseid)
    need (Preprocessing.Index indices caseid)
    need (Preprocessing.AcqParams indices caseid)
    need (Topup.TopupOutput indices caseid)
    need (Topup.Mask indices caseid)
    command_ [] "eddy" ["--imain=" ++ (path $ N.Dwi (N.DwiJoinedAll indices, caseid))
                        ,"--mask=" ++ (path $ Topup.Mask indices caseid)
                        ,"--index=" ++ (path $ Preprocessing.Index indices caseid)
                        ,"--acqp=" ++ (path $ Preprocessing.AcqParams indices caseid)
                        ,"--bvecs=" ++ (FSL.bvec $ N.Dwi (N.DwiJoinedAll indices, caseid))
                        ,"--bvals=" ++ (FSL.bval $ N.Dwi (N.DwiJoinedAll indices, caseid))
                        ,"--fwhm=0"
                        ,"--topup=" ++ (Paths.hcpdir caseid "2_Topup" </> showKey (Topup.TopupOutput indices caseid))
                        ,"--flm=quadratic"
                        ,"-v"
                        ,"--out=" ++ path out]

rules =
  rule (buildNode :: EddyUnwarpedImages -> Maybe (Action [Double]))
