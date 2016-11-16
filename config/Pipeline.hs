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
import qualified FSL (threshold, average)
import qualified Nrrd


type CaseId = String

data ImageType = Bse | TensorMask
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

--------------------------------------------------------------------------------
-- AtlasPair

data ThresholdedMasks = ThresholdedMasks | UnThresholdedMasks
                 deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

data AtlasPair = AtlasPair ThresholdedMasks CaseId CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildKey AtlasPair where
  paths x = [atlasPath Bse x, atlasPath TensorMask x]
    where
      atlasPath imgtype (AtlasPair thresholded caseidT caseid)
        = outdir </> caseidT
        </> (intercalate "-" [show imgtype, show thresholded, caseid, "in", caseidT])
        <.> "nii.gz"

  build out@(AtlasPair UnThresholdedMasks caseidT caseid) = Just $ withTempDir $ \tmpdir -> do
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


  build out@(AtlasPair ThresholdedMasks caseidT caseid) = Just $ withTempDir $ \tmpdir -> do
    let atlas = AtlasPair UnThresholdedMasks caseidT caseid
    apply1 atlas :: Action [Double]
    FSL.threshold 0.5 (last . paths $ atlas) (last . paths $ out)

getAtlasSet caseidT thresholdedMasks = do
    Just trainingCases <- fmap words <$> getConfig "trainingCases"
    let atlases = [ AtlasPair thresholdedMasks caseidT caseid
                  | caseid <- trainingCases ]
    apply atlases :: Action [[Double]]
    return atlases


--------------------------------------------------------------------------------
-- DwiMask

data MaskingAlgorithm = MABS
                      | NNLSFusion Int
                      deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)


data DwiMask = DwiMask MaskingAlgorithm ThresholdedMasks CaseId
             deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildKey DwiMask where
  path x@(DwiMask _ _ caseid) = outdir </> caseid
                                </> (intercalate "-" (words. show $ x)) <.> "nrrd"

  build out@(DwiMask MABS threshType caseid) = Just $ do
    atlases <- getAtlasSet caseid threshType
    let niiOut = replaceExtension (path out) "nii.gz"
    FSL.average niiOut (map (last . paths) atlases)
    FSL.threshold 0.5 niiOut niiOut
    unit $ cmd "ConvertBetweenFileFormats" niiOut (path out)

  build out@(DwiMask (NNLSFusion radius) threshType caseid) = Just $
    withTempFile $ \tmpfile -> do
      atlases <- getAtlasSet caseid threshType
      apply1 (TrainingBse caseid) :: Action [Double]
      let pre = tmpfile
          nnlsOut = tmpfile ++ "_ncc.nii.gz"
      command_ [] "nnlsFusion" ["-r", show radius
                              ,"-F", path (TrainingBse caseid)
                              ,"-A", map (head . path) atlases
                              ,"-S", map (last . path) atlases
                              ,"-O", pre
                              ]
      unit $ cmd "ConvertBetweenFileFormats" nnlsOut (path out)
      unit $ cmd "center.py -i" (path out) "-o" (path out)


--------------------------------------------------------------------------------
-- Dice Coefficient

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=outdir, shakeVerbosity=Chatty} $ do
    usingConfigFile "config/config.cfg"

    want [outdir </> "dicecoefficients.csv"]

    outdir </> "dicecoefficients.csv" %> \out -> do
        need ["config/caselist.txt"]
        caselist <- readFileLines "config/caselist.txt"
        let masks = [DwiMask algo thresh caseid
                    | algo <- [MABS, NNLSFusion 4, NNLSFusion 8]
                    , thresh <- [ThresholdedMasks, UnThresholdedMasks]
                    , caseid <- caselist]
        let manualMasks = [TrainingMask caseid | caseid <- caselist]
        apply masks :: Action [[Double]]
        apply manualMasks :: Action [[Double]]
        let mkrow (m, mT) = do
              coeff <- Nrrd.diceCoefficient (path m) (path mT)
              return $ intercalate "," [show m, show coeff]
        rows <- traverse mkrow $ zip masks manualMasks
        writeFileLines out $ ["mask,coeff"] ++ rows

    rule $ (buildKey :: DwiMask -> Maybe (Action [Double]))
    rule $ (buildKey :: AtlasPair -> Maybe (Action [Double]))
    rule $ (buildKey :: TrainingBse -> Maybe (Action [Double]))
    rule $ (buildKey :: TrainingMask -> Maybe (Action [Double]))
