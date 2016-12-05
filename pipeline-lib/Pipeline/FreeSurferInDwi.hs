{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Pipeline.FreeSurferInDwi
  ( rules
  , FsInDwi (..)
  ) where

import           Data.Foldable           (traverse_)
import qualified Paths
import           Pipeline.ANTs           hiding (rules)
import           Pipeline.DWI            hiding (rules)
import           Pipeline.DWIMask        hiding (rules)
import           Pipeline.FreeSurfer     hiding (rules)
import           Pipeline.Structural     hiding (rules)
import           Pipeline.StructuralMask hiding (rules)
import           Pipeline.Util           (showKey)
import           PipelineRegistrations   (freesurferToDwiWithMasks,
                                          makeRigidMask)
import System.Directory as IO (renameFile)
import           Shake.BuildNode

type CaseId = String

newtype FsInDwi = FsInDwi (StructuralMaskType, DwiType, DwiMaskType, CaseId)
             deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode FsInDwi where
  path n@(FsInDwi (_, _, _, caseid)) = Paths.fsInDwiDir caseid </> showKey n <.> "nii.gz"

  build node@(FsInDwi (strctmaskType, dwitype, dwimaskType, caseid))
    = Just $ withTempDir $ \tmpdir -> do
      Just antsNode <- fmap ANTs <$> getConfig "ANTs-hash"
      need antsNode
      let fsdirN = FreeSurfer (strctmaskType, caseid)
          dwiN = Dwi (dwitype, caseid)
          dwiMaskN = DwiMask (dwimaskType, dwitype, caseid)
          t2N = Structural (T2w, caseid)
          t1N = Structural (T1w, caseid)
          t1MaskN = StructuralMask (strctmaskType, T1w, caseid)
          fsInDwiDir = (dropExtensions $ path node)
      need fsdirN
      need dwiN
      need dwiMaskN
      need t2N
      need t1N
      need t1MaskN
      let t2mask = tmpdir </> "t2mask.nii.gz"
      liftIO $ makeRigidMask (takeDirectory $ path antsNode)
        (path t1MaskN)
        (path t1N)
        (path t2N)
        t2mask
      freesurferToDwiWithMasks (takeDirectory $ path antsNode)
        (pathDir fsdirN)
        (path dwiN) (path dwiMaskN)
        (path t1N) (path t1MaskN)
        (path t2N) t2mask
        fsInDwiDir
      liftIO $ IO.renameFile (fsInDwiDir </> "wmparc-in-dwi.nii.gz") (path node)


rules = rule (buildNode :: FsInDwi -> Maybe (Action [Double]))
