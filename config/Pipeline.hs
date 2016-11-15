{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
import           Data.List                (intercalate)
import           Development.Shake.Config
import           OutputDirectory          (outdir)
import           Shake.BuildKey
import           qualified System.Directory         as IO
-- import           Software.UKFTractography (UKFTractographyExe (..), rules)
import           AntsPath                 (antsPath, antsSrc)
import qualified Intrust


type CaseId = String

data ImageType = Bse | TensorMask
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

data MaskingAlgorithm = NNLSFusion
                      | MABS
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

newtype TrainingMask = TrainingMask CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

toNii nrrd out = unit $ cmd "ConvertBetweenFileFormats" nrrd out

instance BuildKey TrainingMask where
  path (TrainingMask caseid) = outdir </> caseid </> (caseid ++"-tensor-mask")
    <.> "nii.gz"

  build out@(TrainingMask caseid) = Just $ withTempFile $ \tmpfile -> do
    let mask = Intrust.path "dwimask" caseid
        tmpnrrd = tmpfile <.> "nrrd"
    need [mask]
    unit $ cmd "center.py -i" mask "-o" tmpnrrd
    toNii tmpnrrd (path out)

newtype TrainingBse = TrainingBse CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildKey TrainingBse where
  path (TrainingBse caseid) = outdir </> caseid </> (caseid ++ "-bse") <.> "nii.gz"

  build out@(TrainingBse caseid) = Just $ withTempFile $ \tmpf -> do
    let tmpnrrd = tmpf <.> "nrrd"
    let dwied = Intrust.path "dwied" caseid
    need [dwied, replaceExtension dwied "raw.gz"]
    unit $ cmd "bse.sh -i" dwied "-o" tmpnrrd
    unit $ cmd "center.py -i" tmpnrrd "-o" tmpnrrd
    toNii tmpnrrd (path out)


data AtlasPair = AtlasPair CaseId CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildKey AtlasPair where
  paths x = [atlasPath Bse x, atlasPath TensorMask x]
    where
      atlasPath imgtype (AtlasPair caseidT caseid)
        = outdir </> caseidT
        </> (intercalate "-" [show imgtype, caseid, "in", caseidT]) <.> "nii.gz"

  build out@(AtlasPair caseidT caseid) = Just $ withTempDir $ \tmpdir -> do
    let bse = TrainingBse caseid
        bseT = TrainingBse caseidT
    apply1 (TrainingMask caseid) :: Action [Double]
    apply [bse, bseT] :: Action [[Double]]
    let pre = tmpdir </> "bse_to_target"
        bseWarped = pre ++ "Warped.nii.gz"
    command_ [] (antsSrc </> "Scripts/antsRegistrationSyN.sh") ["-d", "3"
                                                              ,"-f", path bseT
                                                              ,"-m", path bse
                                                              ,"-o", pre
                                                              ,"-n", "16"]
    let xfmRigid = pre ++ "0GenericAffine.mat"
        xfmWarp = pre ++ "1Warp.nii.gz"
        xfm = pre ++ "Transform.nii.gz"
    command_ [] (antsPath </> "ComposeMultiTransform") ["3"
                                                      ,xfm
                                                      ,"-R", path bseT
                                                      ,xfmWarp
                                                      ,xfmRigid]

    let [bseOut, maskOut] = paths out
    command_ [] (antsPath </> "antsApplyTransforms") ["-d", "3"
                                                    ,"-i", path (TrainingMask caseid)
                                                    ,"-o", maskOut
                                                    ,"-r", path bseT
                                                    ,"-t", xfm]
    liftIO $ IO.copyFile bseWarped bseOut


main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=outdir, shakeVerbosity=Chatty} $ do
  usingConfigFile "config/all.cfg"

  action $ do
    -- Just caseids <- fmap words <$> getConfig "caselist"
    -- Just targetCases <- fmap words <$> getConfig "targetCases"
    Just trainingCases <- fmap words <$> getConfig "trainingCases"
    need ["config/caselist.txt"]
    cases <- readFileLines "config/caselist.txt"
    apply [ AtlasPair caseidT caseid
          | caseidT <- cases
          , caseid <- trainingCases ] :: Action [[Double]]

  rule $ (buildKey :: AtlasPair -> Maybe (Action [Double]))
  rule $ (buildKey :: TrainingBse -> Maybe (Action [Double]))
  rule $ (buildKey :: TrainingMask -> Maybe (Action [Double]))
