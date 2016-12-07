module ANTs
  (computeRigid
  ,computeWarp
  ,applyTransforms
  ,upsample
  ) where

-- import Software.ANTs (ANTs (..))
import           Control.Monad.Extra (whenM)
import           Data.Maybe          (fromMaybe)
import           Development.Shake   (Action, CmdOption (AddEnv), cmd, liftIO,
                                      unit, withTempDir)
import           System.Directory    as IO (doesDirectoryExist
                                           , copyFile
                                           , createDirectoryIfMissing)
import           System.Environment  (lookupEnv)
import           System.FilePath     ((</>))
import           System.IO.Temp      (withSystemTempDirectory,
                                      withSystemTempFile)
import           System.Process      (callProcess)
import qualified Teem                (center, gzip, isNrrd)
import           Util               (convertImage, maskImage, extractB0)
import           System.FilePath    ((</>), (<.>),takeExtensions)
import           Data.Foldable      (traverse_)
import           Control.Monad      (when)


getAnts :: FilePath -> IO FilePath
getAnts "" = do envPath <- fmap (fromMaybe $ error "getAnts: ANTSPATH not set, set it or call function with a path")
                 (lookupEnv "ANTSPATH")
                case envPath of
                  "" -> error "getAnts: ANTSPATH is set to an empty path."
                  _ -> getAnts envPath
getAnts path = do
  whenM (not <$> IO.doesDirectoryExist path)
    (error $ "getAnts: the path " ++ path ++ " does not exist")
  return path


-- TODO replace ANTS with newer antsRegistration
computeRigid antspath moving fixed outtxt
  = withSystemTempDirectory "" $ \tmpdir -> do
    let pre = tmpdir </> "ants"
        affine = pre ++ "Affine.txt"
    callProcess (antspath </> "ANTS") ["3"
                                      ,"-m", "MI["++fixed++","++moving++",1,32]"
                                      ,"-i", "0",
                                       "-o", pre, "--do-rigid"]
    IO.copyFile affine outtxt

computeWarp antspath moving fixed outwarp
  = withSystemTempDirectory "" $ \tmpdir -> do
    let pre = tmpdir </> "ants"
        affine = pre ++ "0GenericAffine.mat"
        warp = pre ++ "1Warp.nii.gz"
    -- antsRegistrationSyN uses MI for the Rigid and Affine stages,
    -- and CC with radius 4 for the non-linear BSplineSyN stage
    callProcess (antspath </> "antsRegistrationSyN.sh")
      ["-d", "3"
      ,"-f", fixed
      ,"-m", moving
      ,"-o", pre
      ,"-n", "16"]
    callProcess (antspath </> "ComposeMultiTransform")
      ["3", outwarp ,"-R", fixed, warp, affine]

applyTransforms antspath interpolation transforms moving fixed out =
  callProcess (antspath </> "antsApplyTransforms") $
    ["-d", "3"
    ,"-i", moving
    ,"-o", out
    ,"-r", fixed
    ,"-t"] ++ transforms
    ++ (if null interpolation then [] else ["--interpolation", interpolation])

upsample antspath spacings img out
  = callProcess (antspath </> "ResampleImageBySpacing")
  ["3", img, out, unwords . map show $ spacings]


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