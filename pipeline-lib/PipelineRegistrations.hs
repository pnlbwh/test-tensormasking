module PipelineRegistrations
  (makeRigidMask
  ,freesurferToDwi
  ,freesurferToDwiWithMasks
  ) where

import           ANTs (upsample, computeRigid, applyTransforms, computeWarp)
import           Control.Monad      (when)
import           Data.Foldable      (traverse_)
import           Data.Maybe         (fromMaybe)
import           System.Directory   as IO (copyFile, createDirectoryIfMissing)
import           System.Environment (lookupEnv)
import           System.FilePath    ((</>), (<.>),takeExtensions)
import           System.IO.Temp     (withSystemTempDirectory,
                                     withSystemTempFile)
import           System.Process     (callProcess)
import qualified Teem               (center, extractB0, gzip, isNrrd)
import           Util               (convertImage, maskImage, extractB0)
import qualified FreeSurfer  as FS (runCmd)
import Development.Shake (liftIO, Action, unit, cmd, withTempDir, CmdOption( AddEnv ) )

makeRigidMask antsPath mask moving fixed out
  = withSystemTempFile ".txt" $ \tmpxfm _ -> do
      withSystemTempFile ".nrrd" $ \tmpmask _ -> do
        withSystemTempFile ".nrrd" $ \tmpmoving _ -> do
          convertImage mask tmpmask
          convertImage moving tmpmoving
          traverse_ Teem.center [tmpmask, tmpmoving]
          computeRigid antsPath tmpmoving fixed tmpxfm
          applyTransforms antsPath "NearestNeighbor" [tmpxfm] tmpmask fixed out
          when (Teem.isNrrd out) (Teem.gzip out)

freesurferToDwi :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> Action ()
freesurferToDwi antsPath mridir bse t1 t2 outdir = do
  liftIO $ createDirectoryIfMissing True outdir
  fshome <- liftIO $ fromMaybe (error "freesurferToDwi: Set FREESURFER_HOME") <$> lookupEnv "FREESURFER_HOME"
  let brain = outdir </> "brain.nii.gz"
      wmparc = outdir </> "wmparc.nii.gz"
      bse = outdir </> "bse" <.> (takeExtensions bse) -- TODO constrain to nrrd/nii
      fsToT1_rigid = outdir </> "fsToT1-rigid.txt"
      t1ToT2_rigid = outdir </> "t1ToT2-rigid.txt"
      t2ToDwi_warp = outdir </> "t2ToDwi-warp.nii.gz"
      wmparcInDwi = outdir </> "wmparc-in-dwi" <.> (takeExtensions bse) -- TODO
  unit $ cmd (AddEnv "SUBJECTS_DIR" "") (fshome </> "bin" </> "mri_vol2vol")
    ["--mov", mridir </> "brain.mgz"
    ,"--targ", mridir </> "brain.mgz"
    ,"--regheader"
    ,"--o", brain]
  unit $ cmd (AddEnv "SUBJECTS_DIR" "")  (fshome </> "bin" </> "mri_label2vol")
    ["--seg", mridir </> "wmparc.mgz"
    ,"--temp", mridir </> "brain.mgz"
    ,"--regheader", mridir </> "wmparc.mgz"
    ,"--o", wmparc]
  -- Make upsampled DWI b0
  liftIO $ upsample antsPath [1,1,1] bse bse
  liftIO $ computeRigid antsPath brain t1 fsToT1_rigid
  liftIO $ computeRigid antsPath t1 t2 t1ToT2_rigid
  liftIO $ computeWarp antsPath t2 bse t2ToDwi_warp
  liftIO $ applyTransforms antsPath "NearestNeighbor"
    [t2ToDwi_warp, t1ToT2_rigid, fsToT1_rigid]
    wmparc bse wmparcInDwi
  unit $ cmd "ConvertBetweenFileFormats" [wmparcInDwi, wmparcInDwi, "short"]

freesurferToDwiWithMasks antsPath mridir dwi dwimask t1 t1mask t2 t2mask outdir
  = withTempDir $ \tmpdir -> do
  let [bsemasked, t1masked, t2masked] = map (tmpdir </>) ["bsemasked.nii.gz"
                                                         ,"t1masked.nii.gz"
                                                         ,"t2masked.nii.gz"]
  liftIO $ Util.extractB0 dwi (tmpdir </> "bse.nii.gz")
  liftIO $ maskImage (tmpdir </> "bse.nii.gz") dwimask bsemasked
  liftIO $ maskImage t1 t1mask t1masked
  liftIO $ maskImage t2 t2mask t2masked
  freesurferToDwi antsPath mridir bsemasked t1masked t2masked outdir
